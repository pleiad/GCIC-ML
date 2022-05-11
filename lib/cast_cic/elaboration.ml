open Common
(** This module specifies the elaboration from GCIC to CastCIC *)

open Common.Std

type elaboration_error = string
(** An error originating from elaboration  *)

type elaboration = Ast.term * Ast.term
(** The elaboration result  *)

let are_consistent t1 t2 : bool =
  let t1_red = Reduction.reduce t1 in
  let t2_red = Reduction.reduce t2 in
  Ast.alpha_consistent t1_red t2_red

let rec elaborate ctx (term : Gcic.Ast.term) :
    (elaboration, elaboration_error) result =
  let open Gcic.Ast in
  match term with
  | Var x -> (
      match Context.lookup ~key:x ~ctx with
      | Some v -> Ok (Ast.Var x, v)
      | None -> Error "free identifier")
  | Universe i -> Ok (Ast.Universe i, Ast.Universe (i + 1))
  | Prod { id; dom; body } ->
      let* elab_dom, i = elab_univ ctx dom in
      let ext_ctx = Context.add ~key:id ~value:elab_dom ctx in
      let* elab_body, j = elab_univ ext_ctx body in
      Ok
        ( Ast.Prod { id; dom = elab_dom; body = elab_body },
          Ast.Universe (Ast.product_universe_level i j) )
  | Lambda { id; dom; body } ->
      let* elab_dom, _ = elab_univ ctx dom in
      let ext_ctx = Context.add ~key:id ~value:elab_dom ctx in
      let* elab_body, elab_body_ty = elaborate ext_ctx body in
      Ok
        ( Ast.Lambda { id; dom = elab_dom; body = elab_body },
          Ast.Prod { id; dom = elab_dom; body = elab_body_ty } )
  | Unknown i ->
      let unk_ty = Ast.Unknown (Ast.Universe i) in
      Ok (Ast.Unknown unk_ty, unk_ty)
  | App (t, u) ->
      let* t', id, dom, body = elab_prod ctx t in
      let* u' = check_elab ctx u dom in
      Ok (Ast.App (t', u'), (Ast.subst1 id u' body))

and check_elab ctx t (ty : Ast.term) : (Ast.term, elaboration_error) result =
  let* t', ty' = elaborate ctx t in
  if are_consistent ty' ty then
    Ok (Ast.Cast { source = ty'; target = ty; term = t' })
  else Error "terms are not consistent"

and elab_univ ctx term : (Ast.term * int, elaboration_error) result =
  let* t, ty = elaborate ctx term in
  match Reduction.reduce ty with
  | Universe i -> Ok (t, i)
  | Unknown (Universe i) ->
      Ok (Ast.Cast { source = ty; target = Universe (i - 1); term = t }, i - 1)
  | _ -> Error "term does not elaborate to type"

and elab_prod ctx term :
    (Ast.term * Id.Name.t * Ast.term * Ast.term, elaboration_error) result =
  let* t, ty = elaborate ctx term in
  match Reduction.reduce ty with
  | Prod { id; dom; body } -> Ok (t, id, dom, body)
  | Unknown (Universe i) when Ast.cast_universe_level i >= 0 -> (
      let open Ast in
      match germ i HProd with
      | Prod fi as prod_germ ->
          Ok
            ( Cast { source = ty; target = prod_germ; term = t },
              fi.id,
              fi.dom,
              fi.body )
      (* if cast level is gt 0, then germ should never reach this*)
      | _ -> Error "impossible state")
  | _ -> Error "term does not elaborate to product"
