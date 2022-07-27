open Common.Id
open Common.Std
open Common.CastCIC

type typing_context = term Name.Map.t

type type_error =
  [ `Err_not_convertible of term * term
  | `Err_free_identifier of Name.t
  | `Err_not_product of term * term
  | `Err_not_universe of term * term
  | `Err_not_inductive of term * term
  ]

module type Reducer = sig
  type error =
    [ `Err_not_enough_fuel
    | `Err_stuck_term of term
    | `Err_free_const
    ]

  val reduce : term -> (term, [> error ]) result
end

module type Store = sig
  type ctor_info =
    { params : (Name.t * term) list
    ; args : (Name.t * term) list
    ; ind : Name.t
    }

  val find_const : Name.t -> term
  val find_ind_params : Name.t -> (Name.t * term) list
  val find_ctor_info : Name.t -> ctor_info
end

module type CastCICTyping = Main.Typing with type t = term

module Make (ST : Store) (R : Reducer) : Main.Typing = struct
  type t = term

  type errors =
    [ R.error
    | type_error
    ]

  type i = (term, errors) result
  type c = (unit, errors) result

  let are_convertible t1 t2 : (unit, [> type_error ]) result =
    let* v1 = R.reduce t1 in
    let* v2 = R.reduce t2 in
    if alpha_equal v1 v2 then Ok () else Error (`Err_not_convertible (t1, t2))

  let rec infer_type (ctx : typing_context) (t : term) : i =
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
      (try Ok (ST.find_const x) with
      | Not_found -> Error (`Err_free_identifier x))
    | Inductive (ind, i, params) ->
      let params_ty = ST.find_ind_params ind |> subst_tele params in
      let params_with_ty = List.combine params params_ty in
      let* _ = map_results (fun (t, ty) -> check_type ctx t ty) params_with_ty in
      Ok (Universe i)
    | Constructor { ctor; level; args; params } ->
      let ctor_info = ST.find_ctor_info ctor in
      let pargs_ty = subst_tele (params @ args) (ctor_info.params @ ctor_info.args) in
      let params_ty, args_ty = List.split_at (List.length params) pargs_ty in
      let args_with_ty = List.combine args args_ty in
      let params_with_ty = List.combine params params_ty in
      let* _ = map_results (fun (t, ty) -> check_type ctx t ty) args_with_ty in
      let* _ = map_results (fun (t, ty) -> check_type ctx t ty) params_with_ty in
      Ok (Inductive (ctor_info.ind, level, params))
    | Match { discr; z; pred; f; branches; _ } ->
      (* TODO: Missing exhaustiveness check *)
      let* ind, level, params = infer_ind ctx discr in
      let indt = Inductive (ind, level, params) in
      let pred_ctx = Name.Map.add z indt ctx in
      let* _ = infer_univ pred_ctx pred in
      let branch_ctx = Name.Map.add f (Prod { id = z; dom = indt; body = pred }) ctx in
      let* _ = map_results (check_branch branch_ctx z pred params level) branches in
      Ok (subst1 z discr pred)

  and check_type (ctx : typing_context) (t : term) (ty : term)
      : (unit, [> type_error ]) result
    =
    let* ty' = infer_type ctx t in
    are_convertible ty ty'

  and infer_univ (ctx : typing_context) (t : term) : (int, [> type_error ]) result =
    let* ty = infer_type ctx t in
    let* v = R.reduce ty in
    match v with
    | Universe i -> Ok i
    | _ -> Error (`Err_not_universe (t, ty))

  and infer_ind (ctx : typing_context) (t : term)
      : (Name.t * int * term list, [> type_error ]) result
    =
    let* ty = infer_type ctx t in
    let* v = R.reduce ty in
    match v with
    | Inductive (ind, i, params) -> Ok (ind, i, params)
    | _ -> Error (`Err_not_inductive (t, ty))

  and infer_prod (ctx : typing_context) (t : term)
      : (Name.t * term * term, [> type_error ]) result
    =
    let* ty = infer_type ctx t in
    let* v = R.reduce ty in
    match v with
    | Prod { id; dom; body } -> Ok (id, dom, body)
    | _ -> Error (`Err_not_product (t, ty))
  (*
    This function assumes that the constructor in the branch includes all 
    parameters and arguments EXPLICITLY.
*)

  and check_branch ctx z pred params level br =
    let ctor_info = ST.find_ctor_info br.ctor in
    let branch_vars = List.map (fun x -> Var x) br.ids in
    let var_tys = subst_tele branch_vars (ctor_info.params @ ctor_info.args) in
    let vars_w_types = List.combine br.ids var_tys |> List.to_seq in
    let ctx_w_var_types = Name.Map.add_seq vars_w_types ctx in
    (* we need to extract the args separate from the params *)
    let branch_args = List.drop (List.length params) branch_vars in
    let ctor = Constructor { ctor = br.ctor; level; params; args = branch_args } in
    let expected_type = subst1 z ctor pred in
    check_type ctx_w_var_types br.term expected_type
end
