(** This module specifies the typing relation *)

open Ast
open Reduction
open Common.Id
open Common.Std

type typing_context = Ast.term Name.Map.t

type type_error =
  [ `Err_not_convertible of term * term
  | `Err_free_identifier of Name.t
  | `Err_not_product of term * term
  | `Err_not_universe of term * term
  | `Err_not_inductive of term * term
  ]

let string_of_error = function
  | `Err_not_convertible (_t1, _t2) -> "not convertible"
  | `Err_free_identifier _x -> "free identifier"
  | `Err_not_product (_t1, _t2) -> "not a product"
  | `Err_not_universe (_t1, _t2) -> "not a universe"
  | `Err_not_inductive (_t1, _t2) -> "not an inductive"

let are_convertible t1 t2 : (unit, [> type_error ]) result =
  let* v1 = reduce t1 in
  let* v2 = reduce t2 in
  if alpha_equal v1 v2 then Ok () else Error (`Err_not_convertible (t1, t2))

let rec infer_type (ctx : typing_context) (t : term) : (term, [> type_error ]) result =
  match t with
  | Var id ->
    (try Ok (Name.Map.find id ctx) with
    | Not_found -> Error (`Err_free_identifier id))
  | Universe i -> Ok (Universe (i + 1))
  | App (t, u) ->
    let* id, dom, body = infer_prod ctx t in
    let* () = check_type ctx u dom in
    Ok (subst1 id u body)
  | Lambda { id; dom; body } ->
    let* _ = infer_univ ctx dom in
    let* body = infer_type (Name.Map.add id dom ctx) body in
    Ok (Prod { id; dom; body })
  | Prod { id; dom; body } ->
    let* j = infer_univ ctx dom in
    let* i = infer_univ (Name.Map.add id dom ctx) body in
    Ok (Universe (Config.product_universe_level i j))
  | Unknown ty ->
    let* _ = infer_univ ctx ty in
    Ok ty
  | Err ty ->
    let* _ = infer_univ ctx ty in
    Ok ty
  | Cast { source; target; term } ->
    let* _ = infer_univ ctx source in
    let* _ = infer_univ ctx target in
    let* () = check_type ctx term source in
    Ok target
  | Const x ->
    (try Ok (Declarations.Const.find x).ty with
    | Not_found -> Error (`Err_free_identifier x))
  | Inductive (ind, i, params) ->
    let params_ty = (Declarations.Ind.find ind).params |> subst_tele params in
    let params_with_ty = List.combine params params_ty in
    let* _ = map_results (fun (t, ty) -> check_type ctx t ty) params_with_ty in
    Ok (Universe i)
  | Constructor { ctor; level; args; params } ->
    let ctor_info = Declarations.Ctor.find ctor in
    let args_ty = subst_tele args ctor_info.args in
    let params_ty = subst_tele params ctor_info.params in
    let args_with_ty = List.combine args args_ty in
    let params_with_ty = List.combine params params_ty in
    let* _ = map_results (fun (t, ty) -> check_type ctx t ty) args_with_ty in
    let* _ = map_results (fun (t, ty) -> check_type ctx t ty) params_with_ty in
    Ok (Inductive (ctor_info.ind, level, params))
  | Match { discr; z; pred; f; branches; _ } ->
    let* ind, level, params = infer_ind ctx discr in
    let indt = Inductive (ind, level, params) in
    let pred_ctx = Name.Map.add z indt ctx in
    let* _ = infer_univ pred_ctx pred in
    let branch_ctx = Name.Map.add f (Prod { id = z; dom = indt; body = pred }) ctx in
    let* _ = map_results (check_branch branch_ctx z pred params level) branches in
    Ok (subst1 z discr pred)

and check_branch
    (ctx : typing_context)
    (z : Name.t)
    (pred : term)
    (params : term list)
    (level : int)
    (br : branch)
    : (unit, [> type_error ]) result
  =
  let ctor_info = Declarations.Ctor.find br.ctor in
  let all_tys = List.append ctor_info.params ctor_info.args in
  let vars = List.map (fun x -> Var x) br.ids in
  let all_terms = List.append params vars in
  let arg_tys = subst_tele all_terms all_tys |> List.drop (List.length params) in
  let args_ctx = List.combine br.ids arg_tys |> List.to_seq in
  let branch_ctx = Name.Map.add_seq args_ctx ctx in
  let ctor = Constructor { ctor = br.ctor; level; params; args = vars } in
  let ty = subst1 z ctor pred in
  check_type branch_ctx br.term ty

and check_type (ctx : typing_context) (t : term) (ty : term)
    : (unit, [> type_error ]) result
  =
  let* ty' = infer_type ctx t in
  are_convertible ty ty'

and infer_prod (ctx : typing_context) (t : term)
    : (Name.t * term * term, [> type_error ]) result
  =
  let* ty = infer_type ctx t in
  let* v = reduce ty in
  match v with
  | Prod { id; dom; body } -> Ok (id, dom, body)
  | _ -> Error (`Err_not_product (t, ty))

and infer_univ (ctx : typing_context) (t : term) : (int, [> type_error ]) result =
  let* ty = infer_type ctx t in
  let* v = reduce ty in
  match v with
  | Universe i -> Ok i
  | _ -> Error (`Err_not_universe (t, ty))

and infer_ind (ctx : typing_context) (t : term)
    : (Name.t * int * term list, [> type_error ]) result
  =
  let* ty = infer_type ctx t in
  let* v = reduce ty in
  match v with
  | Inductive (ind, i, params) -> Ok (ind, i, params)
  | _ -> Error (`Err_not_inductive (t, ty))
