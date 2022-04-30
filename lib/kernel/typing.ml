(** This module specifies the typing relation *)

open Cast_cic
open Reduction


let (let*) = Result.bind
type type_error = string
let error_msg x = x

let are_convertible ctx t1 t2 : (unit, type_error) result =
  let v1 = reduce_in ctx t1 in
  let v2 = reduce_in ctx t2 in
  if alpha_equal v1 v2 then Ok () else Error "not convertible"

let rec infering (ctx : context) (t : term) : (term, type_error) result =
  match t with
  | Var id -> Option.to_result ~none:"free identifier" (Context.lookup ~key:id ~ctx)
  | Universe i -> Ok (Universe (i + 1))
  | App (t, u) ->
     let* (id, dom, body) = infering_prod ctx t in
     let* () = checking ctx u dom in
     Ok (subst1 id u body)
  | Lambda {id; dom; body} ->
     let* _ = infering_univ ctx dom in
     let* body = infering (Context.add ~key:id ~value:dom ctx) body in
     Ok (Prod {id; dom; body})
  | Prod {id; dom; body} -> 
     let* j = infering_univ ctx dom in
     let* i = infering_univ (Context.add ~key:id ~value:dom ctx) body in
     Ok (Universe (product_universe_level i j))
  | Unknown ty -> let* _ = infering_univ ctx ty in Ok ty
  | Err ty -> let* _ = infering_univ ctx ty in Ok ty
  | Cast {source; target; term} ->
    let* _ = infering_univ ctx source in
    let* _ = infering_univ ctx target in
    let* () = checking ctx term source in
    Ok target

and checking (ctx : context) (t : term) (ty : term) : (unit, type_error) result =
  let* ty' = infering ctx t in 
  are_convertible ctx ty ty'

and infering_prod (ctx : context) (t : term) : (Name.t * term * term, type_error) result =
  let* ty = infering ctx t in
  match reduce_in ctx ty with
  | Prod {id; dom; body} -> Ok (id, dom, body)
  | _                   -> Error "not a product"

and infering_univ (ctx : context) (t : term) : (int, type_error) result =
  let* ty = infering ctx t in
  match reduce_in ctx ty with
  | Universe i -> Ok i
  | _          -> Error "not a universe"