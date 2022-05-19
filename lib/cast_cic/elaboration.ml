(** This module specifies the elaboration from GCIC to CastCIC *)
open Context

open Common.Std
open Common.Id

(** This module specifies the elaboration from GCIC to CastCIC *)

(**********************)
(*       ERRORS       *)
(**********************)

type elaboration_error =
  [ `Err_free_identifier of Name.t
  | `Err_inconsistent of Kernel.Ast.term * Ast.term * Ast.term
  | `Err_constrained_universe of Kernel.Ast.term
  | `Err_constrained_product of Kernel.Ast.term
  | `Err_impossible of Kernel.Ast.term
  ]

let string_of_error err =
  let error_code =
    match err with
    | `Err_free_identifier _ -> "free_id"
    | `Err_inconsistent _ -> "inconsistent_terms"
    | `Err_constrained_universe _ -> "constrained_univ_elab"
    | `Err_constrained_product _ -> "constrained_prod_elab"
    | `Err_impossible _ -> "impossible"
  in
  let message =
    match err with
    | `Err_free_identifier _x -> "free_id"
    | `Err_inconsistent (_term, _ty, _s_ty) -> "terms are not consistent"
    | `Err_constrained_universe _term -> "term does not elaborate to universe"
    | `Err_constrained_product _term -> "term does not elaborate to product"
    | `Err_impossible _ -> "impossible state"
  in
  let term =
    match err with
    | `Err_free_identifier x -> Ast.(Var x |> to_string)
    | `Err_inconsistent (term, _, _) -> Kernel.Ast.to_string term
    | `Err_constrained_universe term -> Kernel.Ast.to_string term
    | `Err_constrained_product term -> Kernel.Ast.to_string term
    | `Err_impossible term -> Kernel.Ast.to_string term
  in
  Format.asprintf "[%s] elaboration of term (%s) failed: %s" error_code term message

(**********************)
(*    ELABORATION     *)
(**********************)

(** Type alias for the elaboration result  *)
type elaboration = Ast.term * Ast.term

let are_consistent t1 t2 : bool =
  let res =
    let* t1_red = Reduction.reduce t1 in
    let* t2_red = Reduction.reduce t2 in
    Ok (Ast.alpha_consistent t1_red t2_red)
  in
  Result.fold ~ok:(fun x -> x) ~error:(fun _ -> false) res

(** The elaboration procedure, as per the paper *)
let rec elaborate ctx (term : Kernel.Ast.term)
    : (elaboration, [> elaboration_error ]) result
  =
  let open Kernel.Ast in
  let open Kernel.Variant in
  match term with
  | Var x ->
    (try Ok (Var x, NameMap.find x ctx) with
    | Not_found -> Error (`Err_free_identifier x))
  | Universe i -> Ok (Universe i, Universe (i + 1))
  | Prod { id; dom; body } ->
    let* elab_dom, i = elab_univ ctx dom in
    let extended_ctx = NameMap.add id elab_dom ctx in
    let* elab_body, j = elab_univ extended_ctx body in
    Ok
      ( Ast.Prod { id; dom = elab_dom; body = elab_body }
      , Ast.Universe (product_universe_level i j) )
  | Lambda { id; dom; body } ->
    let* elab_dom, _ = elab_univ ctx dom in
    let extended_ctx = NameMap.add id elab_dom ctx in
    let* elab_body, elab_body_ty = elaborate extended_ctx body in
    Ok
      ( Ast.Lambda { id; dom = elab_dom; body = elab_body }
      , Ast.Prod { id; dom = elab_dom; body = elab_body_ty } )
  | Unknown i ->
    let unk_ty = Ast.Unknown (Ast.Universe i) in
    Ok (Ast.Unknown unk_ty, unk_ty)
  | App (t, u) ->
    let* t', id, dom, body = elab_prod ctx t in
    let* u' = check_elab ctx u dom in
    Ok (Ast.App (t', u'), Ast.subst1 id u' body)
  (* Extra rules *)
  | Ascription (t, ty) ->
    let* ty', _ = elab_univ ctx ty in
    let* t' = check_elab ctx t ty' in
    Ok (t', ty')
  | UnknownT i -> Ok (Ast.Unknown (Ast.Universe i), Ast.Universe i)

and check_elab ctx term (s_ty : Ast.term) : (Ast.term, [> elaboration_error ]) result =
  let* t', ty = elaborate ctx term in
  if are_consistent ty s_ty
  then Ok (Ast.Cast { source = ty; target = s_ty; term = t' })
  else Error (`Err_inconsistent (term, ty, s_ty))

(* Instead of returning the complete Universe type, we only return the level.
   Otherwise, we need to repeat the pattern-matching/extraction wherever this
   is called *)
and elab_univ ctx term : (Ast.term * int, [> elaboration_error ]) result =
  let* t, ty = elaborate ctx term in
  let* v = Reduction.reduce ty in
  match v with
  (* Inf-Univ *)
  | Universe i -> Ok (t, i)
  (* Inf-Univ? *)
  | Unknown (Universe i) ->
    Ok (Ast.Cast { source = ty; target = Universe (i - 1); term = t }, i - 1)
  | _ -> Error (`Err_constrained_universe term)

(* Similarly to elab_univ, instead of returning the complete product type,
   we only return the constituents of it, ie. its identifier, domain and body.
   Otherwise, we need to repeat the pattern-matching/extraction wherever this
   is called *)
and elab_prod ctx term
    : (Ast.term * Name.t * Ast.term * Ast.term, [> elaboration_error ]) result
  =
  let* t, ty = elaborate ctx term in
  let* v = Reduction.reduce ty in
  match v with
  (* Inf-Prod *)
  | Prod { id; dom; body } -> Ok (t, id, dom, body)
  (* Inf-Prod? *)
  | Unknown (Universe i) when Kernel.Variant.cast_universe_level i >= 0 ->
    let prod_germ = Ast.(germ i HProd) in
    (match prod_germ with
    | Prod fi as prod_germ ->
      Ok (Cast { source = ty; target = prod_germ; term = t }, fi.id, fi.dom, fi.body)
    (* if cast level is gt 0, then germ should never reach this*)
    | _ -> Error (`Err_impossible term))
  | _ -> Error (`Err_constrained_product term)
