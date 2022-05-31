(** This module specifies the operational semantics *)
(* The implementation is based on a CEK machine (https://en.wikipedia.org/wiki/CEK_Machine) *)

open Ast
open Common.Id

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of term
  | `Err_free_const
  ]

let string_of_error = function
  | `Err_not_enough_fuel -> "not enough fuel"
  | `Err_stuck_term _term -> "stuck term"
  | `Err_free_const -> "free constant"

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
    let params = (Declarations.Ind.find ind).params in
    let unk_params = List.map (fun (_, t) -> Unknown t) params in
    Inductive (ind, i, unk_params)

(** Checks if a term corresponds to a germ at the provided universe level *)
let is_germ i : term -> bool = function
  | Prod { id = _; dom = Unknown (Universe j); body = Unknown (Universe k) } ->
    Config.cast_universe_level i = j && j = k && j >= 0
  | Err (Universe j) -> i = j
  | Universe j -> j < i
  | Inductive (ind, _, params) ->
    let param_tys = (Declarations.Ind.find ind).params |> List.map snd in
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

(* Type alias *)
type state = term * continuation list

exception Stuck_term of term
exception Not_enough_fuel

(** One step reduction of terms *)
let rec reduce1 (term, cont) : state =
  match term, cont with
  (* Redexes *)
  | Const x, _ -> (Declarations.Const.find x).term, cont
  (* Beta *)
  | Lambda { id; dom = _; body }, KApp_l u :: cont -> subst1 id u body, cont
  (* Prod-Unk *)
  | Unknown (Prod fi), _ -> Lambda { fi with body = Unknown fi.body }, cont
  (* Prod-Err *)
  | Err (Prod fi), _ -> Lambda { fi with body = Err fi.body }, cont (* Down-Unk *)
  | ( Cast { source = Unknown (Universe i); target; term = Unknown (Unknown (Universe j)) }
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
  (* Head-Err *)
  | Cast { source; target; term = _ }, _
    when is_type source && is_type target && not (equal_head source target) ->
    Err target, cont
  (* Dom-Err *)
  | Cast { source = Err (Universe _); target; term = _ }, _ -> Err target, cont
  (* Codom-Err *)
  | Cast { source; target = Err (Universe _) as tgt; term = _ }, _ when is_type source ->
    Err tgt, cont
  (* Prod-Germ *)
  | Cast { source = Prod _ as src; target = Unknown (Universe i) as tgt; term }, _
    when not (is_germ_for_gte_level i src) ->
    let middle = germ i HProd in
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
  | App (t, u), _ -> t, KApp_l u :: cont
  | Unknown t, _ -> t, KUnknown :: cont
  | Err t, _ -> t, KErr :: cont
  | Cast { source; target; term }, _ -> target, KCast_target (source, term) :: cont
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

let fill_hole1 term = function
  | KApp_l u -> App (term, u)
  | KUnknown -> Unknown term
  | KErr -> Err term
  | KCast_source (target, term') -> Cast { source = term; target; term = term' }
  | KCast_target (source, term') -> Cast { source; target = term; term = term' }
  | KCast_term (source, target) -> Cast { source; target; term }

(** Fills a continuation with the given term *)
let fill_hole term = List.fold_left fill_hole1 term

(** One step reduction *)
let step term =
  let initial_state = term, [] in
  (try Ok (reduce1 initial_state) with
  | Stuck_term t -> Error (`Err_stuck_term t)
  | Not_found -> Error `Err_free_const)
  |> Result.map (fun (t, cont) -> fill_hole t cont)
