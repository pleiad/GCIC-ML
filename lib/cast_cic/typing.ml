(** This module specifies the typing relation *)

open Ast
open Reduction
open Common.Id
open Common.Std
open Context

type type_error = string

let error_msg x = x

let are_convertible t1 t2 : (unit, type_error) result =
  let v1 = reduce t1 in
  let v2 = reduce t2 in
  if alpha_equal v1 v2 then Ok () else Error "not convertible"

let rec infer_type (ctx : context) (t : term) : (term, type_error) result =
  match t with
  | Var id -> (
      try Ok (NameMap.find id ctx) with Not_found -> Error "free identifier")
  | Universe i -> Ok (Universe (i + 1))
  | App (t, u) ->
      let* id, dom, body = infer_prod ctx t in
      let* () = check_type ctx u dom in
      Ok (subst1 id u body)
  | Lambda { id; dom; body } ->
      let* _ = infer_univ ctx dom in
      let* body = infer_type (NameMap.add id dom ctx) body in
      Ok (Prod { id; dom; body })
  | Prod { id; dom; body } ->
      let* j = infer_univ ctx dom in
      let* i = infer_univ (NameMap.add id dom ctx) body in
      Ok (Universe (product_universe_level i j))
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

and check_type (ctx : context) (t : term) (ty : term) :
    (unit, type_error) result =
  let* ty' = infer_type ctx t in
  are_convertible ty ty'

and infer_prod (ctx : context) (t : term) :
    (Name.t * term * term, type_error) result =
  let* ty = infer_type ctx t in
  match reduce ty with
  | Prod { id; dom; body } -> Ok (id, dom, body)
  | _ -> Error "not a product"

and infer_univ (ctx : context) (t : term) : (int, type_error) result =
  let* ty = infer_type ctx t in
  match reduce ty with Universe i -> Ok i | _ -> Error "not a universe"
