open Main
open Common.CastCIC
open Common.Id
open Common.Std

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of term
  | `Err_free_const
  ]

module type Store = sig
  val ind_params : Common.Id.Name.t -> term list
  val const : Common.Id.Name.t -> term
  val ctor_param_args : Name.t -> (Name.t * term) list * (Name.t * term) list
end

module type CastCICRed =
  Reduction with type t = term with type o = (term, reduction_error) result

module Make (ST : Store) : CastCICRed = struct
  type t = term
  type o = (term, reduction_error) result

  (** Head constructors *)
  type head =
    | HProd
    | HUniverse of int
    | HInductive of Name.t

  (** Returns the head constructor of a type *)
  let head : term -> (head, string) result = function
    | Prod _ -> Ok HProd
    | Universe i -> Ok (HUniverse i)
    | Inductive (ind, _, _) -> Ok (HInductive ind)
    | _ -> Error "invalid term to get head constructor"

  (** Returns the least precise type for the given head constructor, 
    at the provided level *)
  let germ i : head -> term = function
    | HProd ->
      let cprod = Config.cast_universe_level i in
      let univ = Universe cprod in
      if cprod >= 0
      then Prod { id = Name.default; dom = Unknown univ; body = Unknown univ }
      else Err univ
    | HUniverse j -> if j < i then Universe j else Err (Universe i)
    | HInductive ind ->
      let params = ST.ind_params ind in
      let unk_params = List.map (fun t -> Unknown t) params in
      Inductive (ind, i, unk_params)

  (** Checks if a term corresponds to a germ at the provided universe level *)
  let is_germ i : term -> bool = function
    | Prod { id = _; dom = Unknown (Universe j); body = Unknown (Universe k) } ->
      Config.cast_universe_level i = j && j = k && j >= 0
    | Err (Universe j) -> i = j
    | Universe j -> j < i
    | Inductive (ind, _, params) ->
      let param_tys = ST.ind_params ind in
      List.for_all2 (fun t ty -> alpha_equal t (Unknown ty)) params param_tys
    | _ -> false

  (** Checks if a term corresponds to a germ for a level >= to the provided universe level.
    We are adding this predicate mostly for the Prod-Germ rule in reduction, 
    where we need to check that the type being cast to ? is not a germ for any 
    j >= i. We cannot build an arbitrary j there, so we resort to this.  
  *)
  let is_germ_for_gte_level i : term -> bool = function
    | Prod { id = _; dom = Unknown (Universe j); body = Unknown (Universe k) } ->
      j >= Config.cast_universe_level i && j = k && j >= 0
    | Err (Universe j) -> j = i && Config.cast_universe_level i < 0
    | Inductive _ as t -> is_germ i t
    | _ -> false

  (** Checks if a term corresponds to a type *)
  let is_type : term -> bool = function
    | Prod _ | Universe _ | Inductive _ -> true
    | _ -> false

  let equal_head t1 t2 : bool =
    match head t1, head t2 with
    | Ok HProd, Ok HProd -> true
    | Ok (HUniverse i), Ok (HUniverse j) -> i = j
    | Ok (HInductive i), Ok (HInductive j) -> i = j
    | _, _ -> assert false

  (** Checks if a term is in neutral form *)
  let rec is_neutral : term -> bool = function
    | Var _ -> true
    | App (t, _)
    | Unknown t
    | Err t
    | Cast { source = Unknown (Universe _); term = t; _ }
    | Cast { source = Universe _; target = t; _ }
    | Cast { source = Prod _; target = Prod _; term = t }
    | Cast { source = Prod _; target = t; _ }
    | Cast { source = Inductive _; target = Inductive _; term = t }
    | Cast { source = Inductive _; target = t; _ }
    | Cast { source = t; target = _; term = _ } -> is_neutral t
    | Match { discr; _ } -> is_neutral discr
    | _ -> false

  (** Checks if a type t makes ?_t or err t canonical *)
  let is_unknown_or_error_canonical term : bool =
    match term with
    | Universe _ | Unknown (Universe _) | Err (Universe _) | Inductive _ -> true
    | _ -> is_neutral term

  (** Checks if a term is in canonical form *)
  let is_canonical : term -> bool = function
    | Universe _ | Lambda _ | Prod _ | Constructor _ | Inductive _ -> true
    | Unknown t -> is_unknown_or_error_canonical t
    | Err t -> is_unknown_or_error_canonical t
    | Cast { source = ty; target = Unknown (Universe i); term = _ } when is_germ i ty ->
      true
    | t -> is_neutral t

  (** The representation of a continuation of the CEK machine *)
  type continuation =
    (* Reducing the lhs of an application *)
    | KApp_l of term
    (* Reducing the type of an unknown *)
    | KUnknown
    (* Reducing the type of an error *)
    | KErr
    (* Reducing the source of a cast *)
    | KCast_source of (term * term)
    (* Reducing the target of a cast. The source and term are stored in the state *)
    | KCast_target of (term * term)
    (* Reducing the term of a cast. The source and target are stored in the state *)
    | KCast_term of (term * term)
    (* Reducing the discriminee of a match. The inductive's name, the z variable, 
   the predicate and the f variable are stored in the state. *)
    | KMatch_discr of (Name.t * Name.t * term * Name.t * branch list)

  (* Type alias *)
  type state = term * continuation list

  exception Stuck_term of term
  exception Not_enough_fuel

  (** One step reduction of terms *)
  let rec reduce1 (term, cont) : state =
    match term, cont with
    (* Redexes *)
    | Const x, _ -> ST.const x, cont
    (* Beta *)
    | Lambda { id; dom = _; body }, KApp_l u :: cont -> subst1 id u body, cont
    (* Iota *)
    | Match ({ ind; discr = Constructor ci; f; branches; _ } as mi), _ ->
      (* Might raise Not_found *)
      let branch = List.find (fun br -> br.ctor = ci.ctor) branches in
      let f_subst =
        let new_id = new_identifier () in
        Lambda
          { id = new_id
          ; dom = Inductive (ind, ci.level, ci.params)
          ; body = Match { mi with discr = Var new_id }
          }
      in
      let body_w_f = subst1 f f_subst branch.term in
      (* This assumes that the constructor in the branch was created with all params 
    and args explicit *)
      let zipped_args = List.combine branch.ids (ci.params @ ci.args) in
      let body_w_args =
        List.fold_left (fun acc (id, arg) -> subst1 id arg acc) body_w_f zipped_args
      in
      body_w_args, cont
    (* Prod-Unk *)
    | Unknown (Prod fi), _ -> Lambda { fi with body = Unknown fi.body }, cont
    (* Prod-Err *)
    | Err (Prod fi), _ -> Lambda { fi with body = Err fi.body }, cont
    (* Match-Unk *)
    | Match { discr = Unknown (Inductive _) as t; z; pred; _ }, _ ->
      Unknown (subst1 z t pred), cont
    (* Match-Err *)
    | Match { discr = Err (Inductive _) as discr; z; pred; _ }, _ ->
      Err (subst1 z discr pred), cont
    (* Ind-Unk *)
    | ( Cast
          { source = Inductive (ind1, i1, _)
          ; target = Inductive (ind2, i2, _) as target
          ; term = Unknown (Inductive _)
          }
      , _ )
    (* Is this necessary??? *)
      when ind1 = ind2 && i1 = i2 -> Unknown target, cont
    (* Ind-Err *)
    | ( Cast
          { source = Inductive (ind1, i1, _)
          ; target = Inductive (ind2, i2, _) as target
          ; term = Err (Inductive _)
          }
      , _ )
    (* Is this necessary??? *)
      when ind1 = ind2 && i1 = i2 -> Err target, cont
    (* Down-Unk *)
    | ( Cast
          { source = Unknown (Universe i); target; term = Unknown (Unknown (Universe j)) }
      , _ )
      when i = j (* is this necessary? *) -> Unknown target, cont
    (* Down-Err *)
    | Cast { source = Unknown (Universe _); target; term = Err (Unknown (Universe _)) }, _
      -> Err target, cont
    (* Prod-Prod *)
    | ( Cast
          { source = Prod source_fi
          ; target = Prod target_fi
          ; term = Lambda { id = _; dom; body }
          }
      , _ ) ->
      let y, x = target_fi.id, source_fi.id in
      let inner_cast = Cast { source = target_fi.dom; target = dom; term = Var y } in
      let inner_body = subst1 x inner_cast body in
      let source_cast =
        Cast { source = target_fi.dom; target = source_fi.dom; term = Var y }
      in
      let new_body =
        Cast
          { source = subst1 x source_cast source_fi.body
          ; target = target_fi.body
          ; term = inner_body
          }
      in
      Lambda { id = y; dom = target_fi.dom; body = new_body }, cont
    (* Univ-Univ *)
    | Cast { source = Universe i; target = Universe j; term }, _ when i == j -> term, cont
    (* Ind-Ind *)
    | ( Cast
          { source = Inductive (ind1, i1, source_params)
          ; target = Inductive (ind2, i2, target_params)
          ; term = Constructor ci
          }
      , _ )
      when ind1 = ind2 && i1 = i2 ->
      let ind_params, ctor_args = ST.ctor_param_args ci.ctor in
      let n_params = List.length source_params in
      let arg_ids = List.map fst ctor_args in
      let arg_vars = List.map (fun x -> Var x) arg_ids in
      let source_args =
        subst_tele (source_params @ ci.args) (ind_params @ ctor_args)
        |> List.drop n_params
      in
      let target_args =
        subst_tele (target_params @ arg_vars) (ind_params @ ctor_args)
        |> List.drop n_params
      in
      let target_args = List.combine arg_ids target_args in
      let cast_arg (casted_args, tys) (source, term) =
        let target = List.hd tys |> snd in
        let casted_arg = Cast { source; term; target } in
        casted_arg :: casted_args, subst1_tele casted_arg tys
      in
      let casted_args, _ =
        List.fold_left cast_arg ([], target_args) (List.combine source_args ci.args)
      in
      Constructor { ci with params = target_params; args = casted_args }, cont
      (* Head-Err *)
    | Cast { source; target; term = _ }, _
      when is_type source && is_type target && not (equal_head source target) ->
      Err target, cont
    (* Dom-Err *)
    | Cast { source = Err (Universe _); target; term = _ }, _ -> Err target, cont
    (* Codom-Err *)
    | Cast { source; target = Err (Universe _) as tgt; term = _ }, _ when is_type source
      -> Err tgt, cont
    (* Prod-Germ *)
    | Cast { source = Prod _ as src; target = Unknown (Universe i) as tgt; term }, _
      when not (is_germ_for_gte_level i src) ->
      let middle = germ i HProd in
      let inner_cast = Cast { source = src; target = middle; term } in
      let outer_cast = Cast { source = middle; target = tgt; term = inner_cast } in
      outer_cast, cont
      (* Ind-Germ *)
    | ( Cast
          { source = Inductive (ind, _, _) as src
          ; target = Unknown (Universe i) as tgt
          ; term
          }
      , _ )
      when not (is_germ_for_gte_level i src) ->
      let middle = germ i (HInductive ind) in
      let inner_cast = Cast { source = src; target = middle; term } in
      let outer_cast = Cast { source = middle; target = tgt; term = inner_cast } in
      outer_cast, cont
    (* Up-Down *)
    | ( Cast
          { source = Unknown (Universe i)
          ; target
          ; term = Cast { source = p; target = Unknown (Universe j); term = t }
          }
      , _ )
    (* Is i == j necessary or type checking ensures? *)
      when i == j && is_germ i p -> Cast { source = p; target; term = t }, cont
    (* TODO: Check if this can be replaced with is_germ_for_gte_level *)
    (* Size-Err Universe *)
    | Cast { source = Universe j; target = Unknown (Universe i) as tgt; term = _ }, _
      when j >= i -> Err tgt, cont
    (* Size-Err Prod *)
    | ( Cast
          { source =
              Prod { id = _; dom = Unknown (Universe j); body = Unknown (Universe k) }
          ; target = Unknown (Universe i) as tgt
          ; term = _
          }
      , _ )
      when j == k && j > Config.cast_universe_level i -> Err tgt, cont
    (* Congruence rules *)
    | term, KUnknown :: cont when is_canonical term -> Unknown term, cont
    | term, KErr :: cont when is_canonical term -> Err term, cont
    | target, KCast_target (source, term) :: cont when is_canonical target ->
      source, KCast_source (target, term) :: cont
    | source, KCast_source (target, term) :: cont when is_canonical source ->
      term, KCast_term (source, target) :: cont
    | term, KCast_term (source, target) :: cont when is_canonical term ->
      Cast { source; target; term }, cont
    | discr, KMatch_discr (ind, z, pred, f, branches) :: cont when is_canonical discr ->
      Match { ind; discr; z; pred; f; branches }, cont
    | App (t, u), _ -> t, KApp_l u :: cont
    | Unknown t, _ -> t, KUnknown :: cont
    | Err t, _ -> t, KErr :: cont
    | Cast { source; target; term }, _ -> target, KCast_target (source, term) :: cont
    | Match { ind; discr; z; pred; f; branches }, _ ->
      discr, KMatch_discr (ind, z, pred, f, branches) :: cont
    | _, _ -> raise (Stuck_term term)

  (** Transitive clousure of reduce1 with fuel *)
  and reduce_fueled (fuel : int) ((term, cont) as s) : term =
    if fuel < 0 && Config.uses_fuel ()
    then raise Not_enough_fuel
    else if is_canonical term && cont = []
    then term
    else reduce_fueled (fuel - 1) (reduce1 s)

  (** Reduces a term *)
  and reduce term : (term, [> reduction_error ]) result =
    let initial_state = term, [] in
    let initial_fuel = Config.initial_fuel () in
    try Ok (reduce_fueled initial_fuel initial_state) with
    | Not_enough_fuel -> Error `Err_not_enough_fuel
    | Stuck_term term -> Error (`Err_stuck_term term)
    | Not_found -> Error `Err_free_const
end
