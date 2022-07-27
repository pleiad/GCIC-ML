open Main
open Ast
open Common.Id
open Common.Std
open Common

type elaboration_error =
  [ `Err_free_identifier of Name.t
  | `Err_inconsistent of GCIC.term * elaborated_term * elaborated_term
  | `Err_constrained_universe of GCIC.term
  | `Err_constrained_product of GCIC.term
  | `Err_constrained_inductive of GCIC.term
  ]

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of elaborated_term
  | `Err_free_const
  ]

type errors =
  [ reduction_error
  | elaboration_error
  ]

module type Reducer = sig
  val reduce : elaborated_term -> (elaborated_term, [> reduction_error ]) result
end

module type Store = sig
  type ind_info =
    { params : (Name.t * elaborated_term) list
    ; level : int
    }

  type ctor_info =
    { params : (Name.t * elaborated_term) list
    ; args : (Name.t * elaborated_term) list
    ; ind : Name.t
    }

  val find_const : Name.t -> elaborated_term
  val find_ind : Name.t -> ind_info
  val find_ctor_info : Name.t -> ctor_info
end

module type Typer = sig
  type t
  type i
  type c

  val check_type : t Name.Map.t -> t -> t -> c
  val infer_type : t Name.Map.t -> t -> i
end

module type CastCICElab =
  Elaboration with type o = (elaborated_term * elaborated_term, errors) result

module Make (ST : Store) (R : Reducer) : CastCICElab = struct
  (* type elaboration = elaborated_term * elaborated_term *)
  type o = (elaborated_term * elaborated_term, errors) result

  (** Head constructors *)
  type head =
    | HProd
    | HInductive of Name.t

  (** Returns the least precise type for the given head constructor, 
  at the provided level *)
  let germ i : head -> elaborated_term = function
    | HProd ->
      let cprod = Config.cast_universe_level i in
      let univ = Universe cprod in
      if cprod >= 0
      then Prod { id = Name.default; dom = Unknown univ; body = Unknown univ }
      else Err univ
    | HInductive ind ->
      let params = (ST.find_ind ind).params |> List.map snd in
      let unk_params = List.map (fun t -> Unknown t) params in
      Inductive (ind, i, unk_params)

  (** Checks if two terms are consistent *)
  let rec are_consistent t1 t2 : bool =
    let res =
      let* t1_red = R.reduce t1 in
      let* t2_red = R.reduce t2 in
      Ok (alpha_consistent t1_red t2_red)
    in
    Result.fold ~ok:(fun x -> x) ~error:(fun _ -> false) res

  and alpha_consistent t1 t2 : bool =
    let open Ast in
    match t1, t2 with
    | Var x, Var y -> x = y
    | Universe i, Universe j -> i = j
    | App (t1, u1), App (t2, u2) -> are_consistent t1 t2 && are_consistent u1 u2
    | Lambda fi1, Lambda fi2 ->
      let x_id = new_identifier () in
      let x = Var x_id in
      let body1 = subst1 fi1.id x fi1.body in
      let body2 = subst1 fi2.id x fi2.body in
      are_consistent fi1.dom fi2.dom && are_consistent body1 body2
    | Prod fi1, Prod fi2 ->
      let x_id = new_identifier () in
      let x = Var x_id in
      let body1 = subst1 fi1.id x fi1.body in
      let body2 = subst1 fi2.id x fi2.body in
      are_consistent fi1.dom fi2.dom && are_consistent body1 body2
    | _, Cast ci2 -> are_consistent t1 ci2.term
    | Cast ci1, _ -> are_consistent ci1.term t2
    | _, Unknown _ -> true
    | Unknown _, _ -> true
    | Const x, Const y -> x = y
    | Inductive (ind1, i1, params1), Inductive (ind2, i2, params2) ->
      ind1 = ind2 && i1 = i2 && List.for_all2 are_consistent params1 params2
    | Constructor c1, Constructor c2 ->
      c1.ctor = c2.ctor
      && c1.level = c2.level
      && List.for_all2 are_consistent c1.args c2.args
      && List.for_all2 are_consistent c1.params c2.params
    | Match m1, Match m2 ->
      let are_consistent_branch b1 b2 =
        let open Ast in
        if List.compare_lengths b1.ids b2.ids = 0
        then (
          let new_ids = List.map (fun _ -> Var (new_identifier ())) b1.ids in
          let ids1 = List.combine b1.ids new_ids in
          let ids2 = List.combine b1.ids new_ids in
          let subst_body body ids =
            List.fold_left
              (fun body (old_id, new_id) -> subst1 old_id new_id body)
              body
              ids
          in
          let subst_body1 = subst_body b1.term ids1 in
          let subst_body2 = subst_body b2.term ids2 in
          b1.ctor = b2.ctor && are_consistent subst_body1 subst_body2)
        else false
      in
      are_consistent m1.discr m2.discr
      && m1.ind = m2.ind
      && are_consistent m1.pred m2.pred
      && List.equal are_consistent_branch m1.branches m2.branches
    | _ -> false

  (** The elaboration procedure, as per the paper *)
  let rec elab ctx (term : GCIC.term) : (elaborated_term * elaborated_term, errors) result
    =
    let open GCIC in
    match term with
    | Var x ->
      (try Ok (Var x, Name.Map.find x ctx) with
      | Not_found -> Error (`Err_free_identifier x))
    | Universe i ->
      Ok (Universe i, Universe (i + 1))
      (* | _ -> Error (`Err_constrained_inductive term) *)
    | Prod { id; dom; body } ->
      let* elab_dom, i = elab_univ ctx dom in
      let extended_ctx = Name.Map.add id elab_dom ctx in
      let* elab_body, j = elab_univ extended_ctx body in
      Ok
        ( Ast.Prod { id; dom = elab_dom; body = elab_body }
        , Ast.Universe (Config.product_universe_level i j) )
    | Lambda { id; dom; body } ->
      let* elab_dom, _ = elab_univ ctx dom in
      let extended_ctx = Name.Map.add id elab_dom ctx in
      let* elab_body, elab_body_ty = elab extended_ctx body in
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
    (* Inductives *)
    | Inductive (ind, i, params) ->
      let params_ty = (ST.find_ind ind).params in
      let* elab_params = check_elab_params ctx params params_ty in
      Ok Ast.(Inductive (ind, i, elab_params), Universe i)
    (* CONS *)
    | Constructor (ctor, pargs) ->
      let cinfo = ST.find_ctor_info ctor in
      let ind = ST.find_ind cinfo.ind in
      let* elab_pargs = check_elab_params ctx pargs (cinfo.params @ cinfo.args) in
      let elab_params, elab_args = List.split_at (List.length cinfo.params) elab_pargs in
      let elab_ctor =
        Ast.Constructor
          { ctor; level = ind.level; params = elab_params; args = elab_args }
      in
      Ok (elab_ctor, Ast.Inductive (cinfo.ind, ind.level, elab_params))
    (* FIX *)
    | Match { ind; discr; z; pred; f; branches } ->
      let* elab_discr, ind', level, params = elab_ind ctx ind discr in
      (* TODO Check that ind matches the one elaborated by elab_ind *)
      assert (ind = ind');
      let elab_ind = Ast.Inductive (ind, level, params) in
      let ctx_w_z = Name.Map.add z elab_ind ctx in
      let* elab_pred, _ = elab_univ ctx_w_z pred in
      let ctx_w_f =
        Name.Map.add f (Ast.Prod { id = z; dom = elab_ind; body = elab_pred }) ctx
      in
      let* elab_branches =
        map_results (check_elab_branch ctx_w_f z elab_pred params level) branches
      in
      let elab_match =
        Ast.Match
          { ind; discr = elab_discr; z; pred = elab_pred; f; branches = elab_branches }
      in
      Ok (elab_match, Ast.subst1 z elab_discr elab_pred)
    (* Extra rules *)
    | Ascription (t, ty) ->
      let* ty', _ = elab_univ ctx ty in
      let* t' = check_elab ctx t ty' in
      Ok (t', ty')
    | UnknownT i -> Ok (Ast.Unknown (Ast.Universe i), Ast.Universe i)
    | Const x ->
      let* ty =
        try Ok (ST.find_const x) with
        | Not_found -> Error (`Err_free_identifier x)
      in
      Ok (Ast.Const x, ty)

  (* CHECK rule from the original paper *)
  and check_elab ctx term (s_ty : elaborated_term) =
    let* t', ty = elab ctx term in
    if are_consistent ty s_ty
    then
      Ok
        (if Ast.alpha_equal ty s_ty
        then t'
        else Ast.Cast { source = ty; target = s_ty; term = t' })
    else Error (`Err_inconsistent (term, ty, s_ty))

  (* Instead of returning the complete Universe type, we only return the level.
Otherwise, we need to repeat the pattern-matching/extraction wherever this
is called *)
  and elab_univ ctx term : (elaborated_term * int, [> elaboration_error ]) result =
    let* t, ty = elab ctx term in
    let* v = R.reduce ty in
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
      : ( elaborated_term * Name.t * elaborated_term * elaborated_term
      , [> elaboration_error ] ) result
    =
    let* t, ty = elab ctx term in
    let* v = R.reduce ty in
    match v with
    (* Inf-Prod *)
    | Prod { id; dom; body } -> Ok (t, id, dom, body)
    (* Inf-Prod? *)
    | Unknown (Universe i) when Config.cast_universe_level i >= 0 ->
      let prod_germ = germ i HProd in
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
  and elab_ind ctx ind term
      : ( elaborated_term * Name.t * int * elaborated_term list, [> elaboration_error ]
      ) result
    =
    let* t, ty = elab ctx term in
    let* v = R.reduce ty in
    match v with
    (* Inf-Ind *)
    | Inductive (ind, i, params) -> Ok (t, ind, i, params)
    (* Inf-Ind? *)
    | Unknown (Universe i) ->
      let ind_germ = germ i (HInductive ind) in
      (match ind_germ with
      | Inductive (ind, i, params) ->
        Ok (Ast.Cast { source = ty; target = ind_germ; term = t }, ind, i, params)
      | _ -> assert false)
    | _ -> Error (`Err_constrained_inductive term)

  (*
Auxiliary function to check each branch during elaboration of a match.
It assumes that the constructor in the branch includes all 
parameters and arguments EXPLICITLY.
*)
  and check_elab_branch ctx z pred params level br =
    let open Ast in
    let ctor_info = ST.find_ctor_info br.ctor in
    let branch_vars = List.map (fun x -> Var x) br.ids in
    (* Substitute all params and args from the branch in the expected types, to obtain all types instantiated to 
 the correct values. *)
    let arg_tys = subst_tele branch_vars (ctor_info.params @ ctor_info.args) in
    let args_ctx = List.combine br.ids arg_tys |> List.to_seq in
    let branch_ctx_w_vars = Name.Map.add_seq args_ctx ctx in
    (* we need to extract the args separate from the params *)
    let branch_args = List.drop (List.length params) branch_vars in
    let ctor = Constructor { ctor = br.ctor; level; params; args = branch_args } in
    let expected_type = subst1 z ctor pred in
    let* term = check_elab branch_ctx_w_vars br.term expected_type in
    Ok { ctor = br.ctor; ids = br.ids; term }

  (* Checks whether the pre-elaborated params of an inductive or constructor 
have the expected types from the elaborated ones (which we obtain at the moment of definition) *)
  and check_elab_params ctx params params_ty =
    (* We check one param at a time, and then substitute in the rest, before proceeding with them as well *)
    let check_elab_param ctx (elab_params, params_ty) param =
      let expected_type = List.hd params_ty |> snd in
      let* elab_param = check_elab ctx param expected_type in
      Ok (elab_param :: elab_params, Ast.subst1_tele elab_param params_ty)
    in
    fold_results (check_elab_param ctx) (Ok ([], params_ty)) params
    |> Result.map (fun (l, _) -> List.rev l)

  let elaborate t = elab Name.Map.empty t
end
