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
  ]

let string_of_error = function
  | `Err_not_convertible (_t1, _t2) -> "not convertible"
  | `Err_free_identifier _x -> "free identifier"
  | `Err_not_product (_t1, _t2) -> "not a product"
  | `Err_not_universe (_t1, _t2) -> "not a universe"

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
  | Inductive _ -> assert false
  | Constructor _ -> assert false
  | Match _ -> assert false

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
