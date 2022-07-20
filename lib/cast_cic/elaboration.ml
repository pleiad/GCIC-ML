(** This module specifies the elaboration from GCIC to CastCIC *)
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
  | `Err_constrained_inductive of Kernel.Ast.term
  ]

let string_of_error err =
  let error_code =
    match err with
    | `Err_free_identifier _ -> "free_id"
    | `Err_inconsistent _ -> "inconsistent_terms"
    | `Err_constrained_universe _ -> "constrained_univ_elab"
    | `Err_constrained_product _ -> "constrained_prod_elab"
    | `Err_constrained_inductive _ -> "constrained_ind_elab"
  in
  let message =
    match err with
    | `Err_free_identifier _x -> "free_id"
    | `Err_inconsistent (_term, ty, s_ty) ->
      Fmt.str
        "elaborated type@ %a@ is not consistent with the expected type@ %a"
        Ast.pp_term
        ty
        Ast.pp_term
        s_ty
    | `Err_constrained_universe _term -> "term does not elaborate to universe"
    | `Err_constrained_product _term -> "term does not elaborate to product"
    | `Err_constrained_inductive _term -> "term does not elaborate to inductive"
  in
  let term =
    match err with
    | `Err_free_identifier x -> Ast.(Var x |> to_string)
    | `Err_inconsistent (term, _, _) -> Kernel.Ast.to_string term
    | `Err_constrained_universe term -> Kernel.Ast.to_string term
    | `Err_constrained_product term -> Kernel.Ast.to_string term
    | `Err_constrained_inductive term -> Kernel.Ast.to_string term
  in
  Format.asprintf "[%s] elaboration of term (%s) failed: %s" error_code term message

(**********************)
(*    ELABORATION     *)
(**********************)

(** Type alias for the elaboration result  *)
type elaboration = Ast.term * Ast.term

let alpha_pred_branch pred b1 b2 =
  let open Ast in
  if List.compare_lengths b1.ids b2.ids = 0
  then (
    let new_ids = List.map (fun _ -> Var (new_identifier ())) b1.ids in
    let ids1 = List.combine b1.ids new_ids in
    let ids2 = List.combine b1.ids new_ids in
    let subst_body body ids =
      List.fold_left (fun body (old_id, new_id) -> subst1 old_id new_id body) body ids
    in
    let subst_body1 = subst_body b1.term ids1 in
    let subst_body2 = subst_body b2.term ids2 in
    b1.ctor = b2.ctor && pred subst_body1 subst_body2)
  else false

(** Checks if two terms are alpha consistent *)
let rec alpha_consistent reduce t1 t2 : bool =
  let open Ast in
  let t1_red = reduce t1 |> Result.get_ok in
  let t2_red = reduce t2 |> Result.get_ok in
  match t1_red, t2_red with
  | Var x, Var y -> x = y
  | Universe i, Universe j -> i = j
  | App (t1, u1), App (t2, u2) ->
    alpha_consistent reduce t1 t2 && alpha_consistent reduce u1 u2
  | Lambda fi1, Lambda fi2 ->
    let x_id = new_identifier () in
    let x = Var x_id in
    let body1 = subst1 fi1.id x fi1.body in
    let body2 = subst1 fi2.id x fi2.body in
    alpha_consistent reduce fi1.dom fi2.dom && alpha_consistent reduce body1 body2
  | Prod fi1, Prod fi2 ->
    let x_id = new_identifier () in
    let x = Var x_id in
    let body1 = subst1 fi1.id x fi1.body in
    let body2 = subst1 fi2.id x fi2.body in
    alpha_consistent reduce fi1.dom fi2.dom && alpha_consistent reduce body1 body2
  | _, Cast ci2 -> alpha_consistent reduce t1 ci2.term
  | Cast ci1, _ -> alpha_consistent reduce ci1.term t2
  | _, Unknown _ -> true
  | Unknown _, _ -> true
  | Const x, Const y -> x = y
  | Inductive (ind1, i1, params1), Inductive (ind2, i2, params2) ->
    ind1 = ind2 && i1 = i2 && List.for_all2 (alpha_consistent reduce) params1 params2
  | Constructor c1, Constructor c2 ->
    c1.ctor = c2.ctor
    && c1.level = c2.level
    && List.for_all2 (alpha_consistent reduce) c1.args c2.args
    && List.for_all2 (alpha_consistent reduce) c1.params c2.params
  | Match m1, Match m2 ->
    let alpha_consistent_branch = alpha_pred_branch (alpha_consistent reduce) in
    alpha_consistent reduce m1.discr m2.discr
    && m1.ind = m2.ind
    && alpha_consistent reduce m1.pred m2.pred
    && List.equal alpha_consistent_branch m1.branches m2.branches
  | _ -> false

let are_consistent reduce t1 t2 : bool = alpha_consistent reduce t1 t2
(* let res =
  in
  Result.fold ~ok:(fun x -> x) ~error:(fun _ -> false) res *)

(** The elaboration procedure, as per the paper *)
let rec elaborate reduce ctx (term : Kernel.Ast.term)
    : (elaboration, [> elaboration_error ]) result
  =
  let open Kernel.Ast in
  match term with
  | Var x ->
    (try Ok (Var x, Name.Map.find x ctx) with
    | Not_found -> Error (`Err_free_identifier x))
  | Universe i -> Ok (Universe i, Universe (i + 1))
  | Prod { id; dom; body } ->
    let* elab_dom, i = elab_univ reduce ctx dom in
    let extended_ctx = Name.Map.add id elab_dom ctx in
    let* elab_body, j = elab_univ reduce extended_ctx body in
    Ok
      ( Ast.Prod { id; dom = elab_dom; body = elab_body }
      , Ast.Universe (Config.product_universe_level i j) )
  | Lambda { id; dom; body } ->
    let* elab_dom, _ = elab_univ reduce ctx dom in
    let extended_ctx = Name.Map.add id elab_dom ctx in
    let* elab_body, elab_body_ty = elaborate reduce extended_ctx body in
    Ok
      ( Ast.Lambda { id; dom = elab_dom; body = elab_body }
      , Ast.Prod { id; dom = elab_dom; body = elab_body_ty } )
  | Unknown i ->
    let unk_ty = Ast.Unknown (Ast.Universe i) in
    Ok (Ast.Unknown unk_ty, unk_ty)
  | App (t, u) ->
    let* t', id, dom, body = elab_prod reduce ctx t in
    let* u' = check_elab reduce ctx u dom in
    Ok (Ast.App (t', u'), Ast.subst1 id u' body)
  (* Inductives *)
  | Inductive (ind, i, params) ->
    let params_ty = (Declarations.Ind.find ind).params in
    let* elab_params = check_elab_params reduce ctx params_ty params in
    Ok Ast.(Inductive (ind, i, elab_params), Universe i)
  (* CONS *)
  | Constructor (ctor, pargs) ->
    let cinfo = Declarations.Ctor.find ctor in
    let ind = Declarations.Ind.find cinfo.ind in
    let* elab_pargs = check_elab_params reduce ctx (cinfo.params @ cinfo.args) pargs in
    let elab_params, elab_args = List.split_at (List.length cinfo.params) elab_pargs in
    (* FIXME: Get proper level (probably from inductive declaration) *)
    let level = ind.level in
    let elab_ctor =
      Ast.Constructor { ctor; level; params = elab_params; args = elab_args }
    in
    Ok (elab_ctor, Ast.Inductive (cinfo.ind, level, elab_params))
  (* FIX *)
  | Match { ind; discr; z; pred; f; branches } ->
    let* elab_discr, ind', level, params = elab_ind reduce ctx ind discr in
    (* TODO Check that ind matches the one elaborated by elab_ind *)
    assert (ind = ind');
    let elab_ind = Ast.Inductive (ind, level, params) in
    let pred_ctx = Name.Map.add z elab_ind ctx in
    let* elab_pred, _ = elab_univ reduce pred_ctx pred in
    let ctx_w_f =
      Name.Map.add f (Ast.Prod { id = z; dom = elab_ind; body = elab_pred }) ctx
    in
    let* elab_branches =
      map_results (check_elab_branch reduce ctx_w_f z elab_pred params level) branches
    in
    let elab_match =
      Ast.Match
        { ind; discr = elab_discr; z; pred = elab_pred; f; branches = elab_branches }
    in
    Ok (elab_match, Ast.subst1 z elab_discr elab_pred)
  (* Extra rules *)
  | Ascription (t, ty) ->
    let* ty', _ = elab_univ reduce ctx ty in
    let* t' = check_elab reduce ctx t ty' in
    Ok (t', ty')
  | UnknownT i -> Ok (Ast.Unknown (Ast.Universe i), Ast.Universe i)
  | Const x ->
    let* ty =
      try Ok (Declarations.Const.find x).ty with
      | Not_found -> Error (`Err_free_identifier x)
    in
    Ok (Ast.Const x, ty)

(*
    This function assumes that the constructor in the branch includes all 
    parameters and arguments EXPLICITLY.
*)
and check_elab_branch reduce ctx z pred params level br =
  let open Ast in
  let ctor_info = Declarations.Ctor.find br.ctor in
  let branch_vars = List.map (fun x -> Var x) br.ids in
  let arg_tys = subst_tele branch_vars (ctor_info.params @ ctor_info.args) in
  let args_ctx = List.combine br.ids arg_tys |> List.to_seq in
  let branch_ctx_w_f = Name.Map.add_seq args_ctx ctx in
  (* we need to extract the args separate from the params *)
  let br_args = List.drop (List.length params) branch_vars in
  let ctor = Constructor { ctor = br.ctor; level; params; args = br_args } in
  let ty = subst1 z ctor pred in
  let* term = check_elab reduce branch_ctx_w_f br.term ty in
  Ok { ctor = br.ctor; ids = br.ids; term }

and check_elab_params reduce ctx params_ty params =
  let check_elab_param reduce ctx (elab_params, params_ty) param =
    let* elab_param = check_elab reduce ctx param (List.hd params_ty |> snd) in
    Ok (elab_param :: elab_params, Ast.subst1_tele elab_param params_ty)
  in
  fold_results (check_elab_param reduce ctx) (Ok ([], params_ty)) params
  |> Result.map (fun (l, _) -> List.rev l)

and check_elab reduce ctx term (s_ty : Ast.term) =
  let* t', ty = elaborate reduce ctx term in
  if are_consistent reduce ty s_ty
  then
    Ok
      (if Ast.alpha_equal ty s_ty
      then t'
      else Ast.Cast { source = ty; target = s_ty; term = t' })
  else Error (`Err_inconsistent (term, ty, s_ty))

(* Instead of returning the complete Universe type, we only return the level.
   Otherwise, we need to repeat the pattern-matching/extraction wherever this
   is called *)
and elab_univ reduce ctx term : (Ast.term * int, [> elaboration_error ]) result =
  let* t, ty = elaborate reduce ctx term in
  let* v = reduce ty in
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
and elab_prod reduce ctx term
    : (Ast.term * Name.t * Ast.term * Ast.term, [> elaboration_error ]) result
  =
  let* t, ty = elaborate reduce ctx term in
  let* v = reduce ty in
  match v with
  (* Inf-Prod *)
  | Prod { id; dom; body } -> Ok (t, id, dom, body)
  (* Inf-Prod? *)
  | Unknown (Universe i) when Config.cast_universe_level i >= 0 ->
    let prod_germ = Reduction.(germ i HProd) in
    (match prod_germ with
    | Prod fi as prod_germ ->
      Ok (Cast { source = ty; target = prod_germ; term = t }, fi.id, fi.dom, fi.body)
    (* if cast level is gt 0, then germ should never reach this*)
    | _ -> assert false)
  | _ -> Error (`Err_constrained_product term)

(* Similarly to elab_univ and elab_prod, instead of returning the complete 
  inductive type, we only return the constituents of it.
   Otherwise, we need to repeat the pattern-matching/extraction wherever this
   is called *)
and elab_ind reduce ctx ind term
    : (Ast.term * Name.t * int * Ast.term list, [> elaboration_error ]) result
  =
  let* t, ty = elaborate reduce ctx term in
  let* v = reduce ty in
  match v with
  (* Inf-Ind *)
  | Inductive (ind, i, params) -> Ok (t, ind, i, params)
  (* Inf-Ind? *)
  | Unknown (Universe i) ->
    let ind_germ = Reduction.(germ i (HInductive ind)) in
    (match ind_germ with
    | Inductive (ind, i, params) ->
      Ok (Ast.Cast { source = ty; target = ind_germ; term = t }, ind, i, params)
    | _ -> assert false)
  | _ -> Error (`Err_constrained_inductive term)
