(** This module specifies the operational semantics *)
(* The implementation is based on a CEK machine (https://en.wikipedia.org/wiki/CEK_Machine) *)

open Ast

(** Checks if a term corresponds to a type *)
let is_type : term -> bool = function Prod _ | Universe _ -> true | _ -> false

let equal_head t1 t2 : bool =
  match (head t1, head t2) with
  | Ok HProd, Ok HProd -> true
  | Ok (HUniverse i), Ok (HUniverse j) -> i = j
  | _, _ -> assert false

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

exception Stuck_term

(** One step reduction of terms *)
let reduce1 (term, cont) : state =
  match (term, cont) with
  (* Redexes *)
  (* Beta *)
  | Lambda { id; dom = _; body }, KApp_l u :: cont ->
      (subst1 id u body, cont) 
  (* Prod-Unk *)
  | Unknown (Prod fi), _ ->
      (Lambda { fi with body = Unknown fi.body }, cont) 
  (* Prod-Err *)
  | Err (Prod fi), _ ->
      (Lambda { fi with body = Err fi.body }, cont) (* Down-Unk *)
  | ( Cast
        {
          source = Unknown (Universe i);
          target;
          term = Unknown (Unknown (Universe j));
        },
      _ )
    when i = j (* is this necessary? *) ->
      (Unknown target, cont) 
  (* Down-Err *)
  | ( Cast
        {
          source = Unknown (Universe _);
          target;
          term = Err (Unknown (Universe _));
        },
      _ ) ->
      (Err target, cont) 
  (* Prod-Prod *)
  | ( Cast
        {
          source = Prod source_fi;
          target = Prod target_fi;
          term = Lambda { id = _; dom; body };
        },
      _ ) ->
      let y, x = (target_fi.id, source_fi.id) in
      let inner_cast =
        Cast { source = target_fi.dom; target = dom; term = Var y }
      in
      let inner_body = subst1 x inner_cast body in
      let source_cast =
        Cast { source = target_fi.dom; target = source_fi.dom; term = Var y }
      in
      let new_body =
        Cast
          {
            source = subst1 x source_cast source_fi.body;
            target = target_fi.body;
            term = inner_body;
          }
      in
      (Lambda { id = y; dom = target_fi.dom; body = new_body }, cont)
  (* Univ-Univ *)
  | Cast { source = Universe i; target = Universe j; term }, _ when i == j ->
      (term, cont) 
  (* Head-Err *)
  | Cast { source; target; term = _ }, _
    when is_type source && is_type target && not (equal_head source target) ->
      (Err target, cont) 
  (* Dom-Err *)
  | Cast { source = Err (Universe _); target; term = _ }, _ ->
      (Err target, cont) 
  (* Codom-Err *)
  | Cast { source; target = Err (Universe _) as tgt; term = _ }, _
    when is_type source ->
      (Err tgt, cont) 
  (* Prod-Germ *)
  | ( Cast { source = Prod _ as src; target = Unknown (Universe i) as tgt; term },
      _ )
    when not (is_germ_for_gte_level i src) ->
      let middle = germ i HProd in
      let inner_cast = Cast { source = src; target = middle; term } in
      let outer_cast =
        Cast { source = middle; target = tgt; term = inner_cast }
      in
      (outer_cast, cont)
  (* Up-Down *)
  | ( Cast
        {
          source = Unknown (Universe i);
          target;
          term = Cast { source = p; target = Unknown (Universe j); term = t };
        },
      _ )
  (* Is i == j necessary or type checking ensures? *)
    when i == j && is_germ i p ->
      (Cast { source = p; target; term = t }, cont)
      (* TODO: Check if this can be replaced with is_germ_for_gte_level *)
  (* Size-Err Universe *)
  | ( Cast
        { source = Universe j; target = Unknown (Universe i) as tgt; term = _ },
      _ )
    when j >= i ->
      (Err tgt, cont)
  (* Size-Err Prod *)
  | ( Cast
        {
          source =
            Prod
              {
                id = _;
                dom = Unknown (Universe j);
                body = Unknown (Universe k);
              };
          target = Unknown (Universe i) as tgt;
          term = _;
        },
      _ )
    when j == k && j > cast_universe_level i ->
      (Err tgt, cont)

  (* Congruence rules *)
  | term, KUnknown :: cont when is_canonical term -> (Unknown term, cont)
  | term, KErr :: cont when is_canonical term -> (Err term, cont)
  | target, KCast_target (source, term) :: cont when is_canonical target ->
      (source, KCast_source (target, term) :: cont)
  | source, KCast_source (target, term) :: cont when is_canonical source ->
      (term, KCast_term (source, target) :: cont)
  | term, KCast_term (source, target) :: cont when is_canonical term ->
      (Cast { source; target; term }, cont)
  | App (t, u), _ -> (t, KApp_l u :: cont)
  | Unknown t, _ -> (t, KUnknown :: cont)
  | Err t, _ -> (t, KErr :: cont)
  | Cast { source; target; term }, _ ->
      (target, KCast_target (source, term) :: cont)
  | _, _ -> raise Stuck_term

(** Transitive clousure of reduce1 with fuel *)
let rec reduce_fueled (fuel : int) ((term, cont) as s) : term =
  if fuel < 0 then failwith "not enough fuel"
  else if is_canonical term && cont = [] then term
  else reduce_fueled (fuel - 1) (reduce1 s)


(** Reduces a term *)
let reduce term : term = 
  let initial_state = (term, []) in
  reduce_fueled 10000 initial_state

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
  let initial_state = (term, []) in
  (try Ok (reduce1 initial_state) with Stuck_term -> Error "stuck_term")
  |> Result.map (fun (t, cont) -> fill_hole t cont)
