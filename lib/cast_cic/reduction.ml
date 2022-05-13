(** This module specifies the operational semantics *)
(* The implementation is based on a CEK machine (https://en.wikipedia.org/wiki/CEK_Machine) *)

open Ast
open Common

(** Extended AST with tagged values

    We are using this representation because otherwise we have to 
    constantly query whether a term is canonical, when reducing the stack *)
(* type vterm =
  | Var of Id.Name.t
  | Universe of int
  | App of vterm * vterm
  | Lambda of vfun_info
  | Prod of vfun_info
  | Unknown of vterm
  | Err of vterm
  | Cast of vcast_info
  (* Values *)
  | VLambda of vfun_info * vcontext
  | VProd of vfun_info * vcontext
  | VUnknown of vterm
  | VErr of vterm
  | VCast of vcast_info

and vfun_info = { id : Id.Name.t; dom : vterm; body : vterm }
and vcontext = (Id.Name.t, vterm) Context.t
and vcast_info = { source : vterm; target : vterm; term : vterm }

(* This could probably be "smarter", checking if the terms are canonical and
   producing a more accurate tagged term. It would probably require a context
   as well (e.g. for closures). *)

(** Converts a term of the original AST into a term with tagged values *)
let rec to_vterm : term -> vterm = function
  | Var x -> Var x
  | Universe i -> Universe i
  | App (t, u) -> App (to_vterm t, to_vterm u)
  | Lambda fi -> Lambda (to_vfun_info fi)
  | Prod fi -> Prod (to_vfun_info fi)
  | Unknown t -> Unknown (to_vterm t)
  | Err t -> Err (to_vterm t)
  | Cast ci ->
      Cast
        {
          source = to_vterm ci.source;
          target = to_vterm ci.target;
          term = to_vterm ci.term;
        }

and to_vfun_info { id; dom; body } =
  { id; dom = to_vterm dom; body = to_vterm body }

(** Converts a term of the original AST into a term with tagged values *)
let to_vcontext (ctx : context) : vcontext =
  Context.to_list ctx
  |> List.map (fun (k, v) -> (k, to_vterm v))
  |> Context.of_list

(** Converts a term with tagged values into a term of the original AST *)
let rec of_vterm : vterm -> Ast.term = function
  | Var x -> Var x
  | Universe i -> Universe i
  | App (t, u) -> App (of_vterm t, of_vterm u)
  | Lambda fi | VLambda (fi, _) -> Lambda (of_vfun_info fi)
  | Prod fi | VProd (fi, _) -> Prod (of_vfun_info fi)
  | Unknown t | VUnknown t -> Unknown (of_vterm t)
  | Err t | VErr t -> Err (of_vterm t)
  | Cast ci ->
      Cast
        {
          source = of_vterm ci.source;
          target = of_vterm ci.target;
          term = of_vterm ci.term;
        }
  | VCast ci ->
      Cast
        {
          source = of_vterm ci.source;
          target = of_vterm ci.target;
          term = of_vterm ci.term;
        }

and of_vfun_info { id; dom; body } =
  { id; dom = of_vterm dom; body = of_vterm body }

(** Performs substitution of a context inside a vterm.
      Values are untagged since terms in the context may not be fully reduced (see rule Prod-Prod in reduce1) *)
let rec subst ctx : vterm -> vterm = function
  | Var y -> (
      match Context.lookup ~key:y ~ctx with None -> Var y | Some t -> t)
  | Universe i -> Universe i
  | App (t, u) -> App (subst ctx t, subst ctx u)
  | Lambda fi ->
      let y = new_identifier () in
      let ctx' = Context.add ~key:fi.id ~value:(Var y) ctx in
      Lambda { id = y; dom = subst ctx fi.dom; body = subst ctx' fi.body }
  | Prod fi ->
      let y = new_identifier () in
      let ctx' = Context.add ~key:fi.id ~value:(Var y) ctx in
      Prod { id = y; dom = subst ctx fi.dom; body = subst ctx' fi.body }
  | Unknown t | VUnknown t -> Unknown (subst ctx t)
  | Err t | VErr t -> Err (subst ctx t)
  | Cast { source; target; term = term' }
  | VCast { source; target; term = term' } ->
      Cast
        {
          source = subst ctx source;
          target = subst ctx target;
          term = subst ctx term';
        }
  | VLambda (fi, ctx') ->
      let y = new_identifier () in
      let ctx'' = Context.add ~key:fi.id ~value:(Var y) ctx' in
      Lambda { id = y; dom = subst ctx' fi.dom; body = subst ctx'' fi.body }
  | VProd (fi, ctx') ->
      let y = new_identifier () in
      let ctx'' = Context.add ~key:fi.id ~value:(Var y) ctx' in
      Prod { id = y; dom = subst ctx' fi.dom; body = subst ctx'' fi.body } *)

(** Checks if a term corresponds to a type *)
let is_type : term -> bool = function
  | Prod _ | Universe _ -> true
  | _ -> false

let equal_head t1 t2 : bool = 
  match head t1, head t2 with 
  | Ok HProd, Ok HProd -> true 
  | Ok (HUniverse i), Ok (HUniverse j) -> i = j 
  | _, _ -> assert false   

(** Checks if a term corresponds to a tagged value *)
(* let is_value = function
  | VUnknown (VProd _) | VErr (VProd _) -> false
  | Universe _ | VLambda _ | VProd _ | VUnknown _ | VErr _ | VCast _ -> true
  | _ -> false *)

(** The representation of a continuation of the CEK machine *)
type continuation =
  | KHole
  (* Reducing the rhs of an app *)
  | KApp_l of (term * context * continuation)
  (* Reducing the lhs of an app *)
  (* | KApp_r of (vfun_info * context * continuation) *)
  (* Reducing the domain of a lambda *)
  (* | KLambda of (Id.Name.t * term * context * continuation) *)
  (* Reducing the domain of a product *)
  (* | KProd of (Id.Name.t * term * context * continuation) *)
  (* Reducing the type of an unknown *)
  | KUnknown of (context * continuation)
  (* Reducing the type of an error *)
  | KErr of (context * continuation)
  (* Reducing the source of a cast *)
  | KCast_source of (term * term * context * continuation)
  (* Reducing the target of a cast. Source and term are stored in the state *)
  | KCast_target of (term * term * context * continuation)
  (* Reducing the term of a cast. Source and target are stored in the state *)
  | KCast_term of (term * term * context * continuation)

(* Just an alias *)
type state = term * context * continuation

exception Stuck_term

(** One step reduction of terms *)
let reduce1 (term, ctx, cont) : state =
  match (term, cont) with
  (* Redexes *)
    (* Beta *)
  | Lambda {id; dom=_; body}, KApp_l (u, ctx, cont) -> 
    (subst1 id u body, ctx, cont)

      (* Prod-Unk *)
  | Unknown (Prod fi), _ ->
      (Lambda { fi with body = Unknown fi.body }, ctx,        cont )
      (* Prod-Err *)
  | Err (Prod fi), _ ->
      ( Lambda { fi with body = Err fi.body },
        ctx,
        cont )
      (* Down-Unk *)
  | Cast { source= Unknown (Universe i); target; term= Unknown (Unknown (Universe j))},
      _ when i = j (* is this necessary? *)  ->
      (Unknown target, ctx, cont) 
      (* Down-Err *)
  | Cast { source= Unknown (Universe _); target; term= Err (Unknown (Universe _))},
      _ ->
      (Err target, ctx, cont) 
      (* Prod-Prod *)
  | Cast { source= Prod source_fi; target= Prod target_fi; term = Lambda {id=_; dom; body}}, _ -> 
    let y, x = target_fi.id, source_fi.id in
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
    (Lambda { id = y; dom = target_fi.dom; body=new_body }, ctx, cont)

    (* Univ-Univ *)
  | Cast {source= Universe i; target=Universe j; term}, _ when i == j ->
      (term, ctx, cont) 
    (* Head-Err *)
  | Cast {source; target; term=_}, _ 
    when is_type source && is_type target && not (equal_head source target) ->
      (Err target, ctx, cont) 
      (* Dom-Err *)
  | Cast {source= Err (Universe _); target; term=_}, _ ->  (Err target, ctx, cont) 
      (* Codom-Err *)
  | Cast {source; target=(Err (Universe _)) as tgt; term=_}, _ when is_type source ->
      (Err tgt, ctx, cont) 
      (* Prod-Germ *)
  | Cast {source= (Prod _) as src; target=(Unknown (Universe i)) as tgt; term}, _ 
    when not (is_germ_for_gte_level i src) ->
      let middle = germ i HProd in
      let inner_cast = Cast { source=src; target = middle; term } in
      let outer_cast = Cast { source = middle; target=tgt; term = inner_cast } in
      (outer_cast, ctx, cont)
      (* Up-Down *)
  | Cast {source= Unknown (Universe i);
          target; 
          term=Cast { source=p; target=Unknown (Universe j); term=t}}, _
  (* Is i == j necessary or type checking ensures? *)
    when i == j && is_germ i p ->
      (Cast { source=p; target; term=t }, ctx, cont)

    (* TODO: Check if this can be replaced with is_germ_for_gte_level *)
    (* Size-Err Universe *)
  | Cast {source=Universe j; target= (Unknown (Universe i)) as tgt; term=_}, _ 
   when j >= i ->
      (Err tgt, ctx, cont)
  (* Size-Err Prod *)
  | Cast
        {source= Prod
            {
                id = _;
                dom = Unknown (Universe j);
                body = Unknown (Universe k);
              }              ;
          target=(Unknown (Universe i)) as tgt;
          term=_}, _
    when j == k && j > cast_universe_level i ->
      (Err tgt, ctx, cont)

  (* Congruence rules *)
  | t, KUnknown (_, cont) when is_canonical t -> (Unknown t, ctx, cont)
  | t, KErr (_, cont) when is_canonical t -> (Err t, ctx, cont)
  | target, KCast_target (source, term, _, cont) when is_canonical target ->
      (source, ctx, KCast_source (target, term, ctx, cont))
  | source, KCast_source (target, term, _, cont) when is_canonical source ->
        (term, ctx, KCast_term (source, target, ctx, cont))
  | term, KCast_term (source, (Unknown (Universe i) as target), _, cont)
        when is_germ i source ->
          (Cast { source; target; term }, ctx, cont)
  | App (t, u), _ -> (t, ctx, KApp_l (u, ctx, cont))
  | Unknown t, _ -> (t, ctx, KUnknown (ctx, cont))
  | Err t, _ -> (t, ctx, KErr (ctx, cont))
  | Cast { source; target; term }, _ ->
    (target, ctx, KCast_target (source, term, ctx, cont))      
        (* | dom, KLambda (id, body, _, cont) when is_value dom ->
            (VLambda ({ id; dom; body }, ctx), ctx, cont) *)
        (* | dom, KProd (id, body, _, cont) when is_value dom ->
            (VProd ({ id; dom; body }, ctx), ctx, cont) *)
  (* | Lambda fi, _ -> (fi.dom, ctx, KLambda (fi.id, fi.body, ctx, cont)) *)
  (* | Prod fi, _ -> (fi.dom, ctx, KProd (fi.id, fi.body, ctx, cont)) *)
  
  | _, _ -> raise Stuck_term

(** Transitive clousure of reduce1 with fuel *)
let rec reduce_fueled (fuel : int) ((term, _, cont) as s) : term =
  if fuel < 0 then failwith "not enough fuel"
  else if is_canonical term && cont == KHole then term
  else reduce_fueled (fuel - 1) (reduce1 s)

(** Reduces a term in the given context *)
let reduce_in ctx t : term =
  (* let t' = to_vterm t in *)
  (* let ctx' = to_vcontext ctx in *)
  let initial_state = (t, ctx, KHole) in
  reduce_fueled 10000 initial_state

(** Reduces a term *)
let reduce : term -> term = reduce_in Context.empty

(** Fills a continuation with the given term *)
let rec fill_hole term cont =
  match cont with
  | KHole -> term
  | KApp_l (u, _, cont) -> fill_hole (App (term, u)) cont
  (* | KApp_r (t, _, cont) -> fill_hole (App (Lambda t, term)) cont *)
  (* | KLambda (id, body, _, cont) ->
      fill_hole (Lambda { id; dom = term; body }) cont *)
  (* | KProd (id, body, _, cont) -> fill_hole (Prod { id; dom = term; body }) cont *)
  | KUnknown (_, cont) -> fill_hole (Unknown term) cont
  | KErr (_, cont) -> fill_hole (Err term) cont
  | KCast_source (target, term', _, cont) ->
      fill_hole (Cast { source = term; target; term = term' }) cont
  | KCast_target (source, term', _, cont) ->
      fill_hole (Cast { source; target = term; term = term' }) cont
  | KCast_term (source, target, _, cont) ->
      fill_hole (Cast { source; target; term }) cont

(** One step reduction *)
let step ctx term =
  (* let vt = to_vterm term in *)
  (* let vctx = to_vcontext ctx in *)
  let s = (term, ctx, KHole) in
  (try Ok (reduce1 s) with Stuck_term -> Error "stuck_term")
  |> Result.map (fun (t, _, cont) -> fill_hole t cont)
