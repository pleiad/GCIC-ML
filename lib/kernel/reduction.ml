(** This module specifies the operational semantics *)
(* The implementation is based on a CEK machine (https://en.wikipedia.org/wiki/CEK_Machine) *)

open Cast_cic

(** Extended AST with tagged values

    We are using this representation because otherwise we have to 
    constantly query whether a term is canonical, when reducing the stack *)
type vterm = 
  | Var of Name.t
  | Universe of int
  | App of vterm * vterm
  | Lambda of vfun_info
  | Prod of vfun_info
  | Unknown of vterm
  | Err of vterm
  | Cast of { source: vterm; target: vterm; term: vterm }
  | VLambda of vfun_info * vcontext
  | VProd of vfun_info * vcontext
  | VUnknown of vterm
  | VErr of vterm
and vfun_info = { id: Name.t; dom: vterm; body: vterm }
and vcontext = (Name.t, vterm) Context.t

(** Converts a term of the original AST into a term with tagged values *)
let rec to_vterm : term -> vterm = function
  | Var x -> Var x
  | Universe i -> Universe i
  | App (t, u) -> App (to_vterm t, to_vterm u)
  | Lambda fi -> Lambda (to_vfun_info fi)
  | Prod fi -> Prod (to_vfun_info fi)
  | Unknown t -> Unknown (to_vterm t)
  | Err t -> Err (to_vterm t)
  | Cast ci -> Cast { source=to_vterm ci.source;
                      target=to_vterm ci.target;
                      term=to_vterm ci.term }

and to_vfun_info {id; dom; body} = {id; dom=to_vterm dom; body=to_vterm body}

(** Converts a term with tagged values into a term of the original AST *)
let rec from_vterm : vterm -> term = function
  | Var x -> Var x
  | Universe i -> Universe i
  | App (t, u) -> App (from_vterm t, from_vterm u)
  | Lambda fi | VLambda (fi,_) -> Lambda (from_vfun_info fi)
  | Prod fi  | VProd (fi, _) -> Prod (from_vfun_info fi)
  | Unknown t | VUnknown t -> Unknown (from_vterm t)
  | Err t  | VErr t -> Err (from_vterm t)
  | Cast ci -> Cast { source=from_vterm ci.source;
                      target=from_vterm ci.target;
                      term=from_vterm ci.term }
and from_vfun_info {id; dom; body} = {id; dom=from_vterm dom; body=from_vterm body}

(** Checks if a term corresponds to a type *)
let is_type : vterm -> bool = function
| VProd _ | Universe _ -> true
| _ -> false

(** Checks if a term corresponds to a tagged value *)
let is_value = function
| Universe _ | VLambda _ | VProd _ | VUnknown _ | VErr _ -> true
| _ -> false

(** Untags values *)
let rec from_value : vterm -> vterm = function
| VLambda (fi, _) -> Lambda {fi with dom=from_value fi.dom}
| VProd (fi, _) -> Prod {fi with dom=from_value fi.dom}
| VUnknown t -> Unknown (from_value t)
| VErr t -> Err (from_value t)
| t -> t

(** The representation of a continuation of the CEK machine *)
type continuation =
| KHole
| KApp_l of (vterm * vcontext * continuation)
| KApp_r of (vfun_info * vcontext * continuation)
| KLambda of (Name.t * vterm * vcontext * continuation)
| KProd of (Name.t * vterm * vcontext * continuation)
| KUnknown of (vcontext * continuation)
| KErr of (vcontext * continuation)
| KCast_source of (vterm * vterm * vcontext * continuation)
| KCast_target of (vterm * vterm * vcontext * continuation)
| KCast_term of (vterm * vterm * vcontext * continuation)

(* Just an alias *)
type state = vterm * vcontext * continuation

(** Build the initial state from a term *)
let initial_state t = (t, Context.empty, KHole)

(** One step reduction of terms *)
let reduce1 (term, ctx, cont) : state =
  match term, cont with
  (* Redexes *)
    (* Delta *)
  | (Var x, _) ->
    (match Context.lookup ~key:x ~ctx with
     | Some v -> (v, ctx, cont)
     | None   -> failwith ("free identifier: " ^ Name.to_string x))
     (* Beta *)
  | (u, KApp_r (fi, ctx, cont)) when is_value u ->
    let ctx' = Context.add ~key:fi.id ~value:(from_value u) ctx in
    (fi.body, ctx', cont)
    (* Prod-Unk *)
  | (VUnknown (VProd (fi, ctx')), _) ->
    (VLambda ({id=fi.id; dom=fi.dom; body=Unknown fi.body}, ctx') , ctx, cont)
     (* Prod-Err *)
  | (VErr (VProd (fi, ctx')), _) ->
    (VLambda ({id=fi.id; dom=fi.dom; body=Err fi.body}, ctx'), ctx, cont)
    (* Down-Unk *)
  | (VUnknown (VUnknown (Universe _)), KCast_term (VUnknown (Universe _), target, _, cont)) ->
    (VUnknown target, ctx, cont)
    (* Down-Err *)
  | (VErr (VUnknown (Universe _)), KCast_term (VUnknown (Universe _), target, _, cont)) ->
    (VErr target, ctx, cont)
    (* Prod-Prod
  | (VLambda (term_fi, term_ctx), KCast_term (VProd (source_fi, source_ctx), VProd (target_fi, target_ctx), _, cont)) ->
    let y = Var target_fi.id in (* TODO: Should be a new identifier *)
    let inner_cast = Cast {source=target_fi.dom; target=term_fi.dom; term=y} in
    let outer_cast = Cast {source=target_fi.dom; target=source_fi.dom; term=y} in
    let inner_ctx = Context.add ~key:term_fi.id ~value:inner_cast term_ctx in
    let body = y  (* TODO *) in
    let fi = {id=term_fi.id; dom=target_fi.dom; body} in
    (VLambda (fi, term_ctx), ctx, cont)
    *)
  
    (* Univ-Univ *)
  | (t, KCast_term (Universe i, Universe j, _, cont)) when is_value t && i == j ->
    (t, ctx, cont)
    (* Head-Err *)
  | (t, KCast_term (source, target, _, cont)) when is_value t && is_type source && is_type target ->
     (VErr target, ctx, cont)
    (* Dom-Err *)
  | (t, KCast_term (target, VErr (Universe _), _, cont)) when is_value t -> (VErr target, ctx, cont)
    (* Codom-Err *)
  | (t, KCast_term ((VErr (Universe _) as source), target, _, cont)) when is_value t && is_type target ->
     (VErr source, ctx, cont)
    (* Prod-Germ *)
  | (f, KCast_term ((VProd _ as source), (VUnknown (Universe i) as target), _, cont)) when is_value f ->
    let middle = to_vterm (germ i HProd) in
    let inner_cast = Cast {source; target=middle; term=f} in
    let outer_cast = Cast {source=middle; target; term=inner_cast} in
    (outer_cast, ctx, cont)
    (* Up-Down *)
  | (t, KCast_term (source, Unknown (Universe i), _, (KCast_term (Unknown (Universe j), target, _, cont))))
    when is_value t && i == j && is_germ i (from_vterm source) ->
    (Cast {source; target; term=t}, ctx, cont)
    (* Size-Err
  | (t, KCast_term (source, Unknown (Universe i), _, cont))
    *)
    
  (* Congruence rules *)
  | (dom, KLambda (id, body, _, cont)) when is_value dom -> (VLambda ({id;dom;body}, ctx), ctx, cont)
  | (dom, KProd (id, body, _, cont)) when is_value dom -> (VProd ({id;dom;body}, ctx), ctx, cont)
  | (VLambda (fi, ctx'), KApp_l (u, _, cont)) -> (u, ctx', KApp_r (fi, ctx, cont))
  | (t, KUnknown (_, cont)) when is_value t -> (VUnknown t, ctx, cont)
  | (t, KErr (_, cont)) when is_value t -> (VErr t, ctx, cont)
  | (target, KCast_target (source, term, _, cont)) when is_value target ->
    (source, ctx, KCast_source (target, term, ctx, cont))
  | (source, KCast_source (target, term, _, cont)) when is_value source ->
    (term, ctx, KCast_term (source, target, ctx, cont))

  | (App (t, u), _) -> (t, ctx, KApp_l (u, ctx, cont))
  | (Lambda fi, _) -> (fi.dom, ctx, KLambda (fi.id, fi.body, ctx, cont))
  | (Prod fi, _) -> (fi.dom, ctx, KProd (fi.id, fi.body, ctx, cont))
  | (Unknown t, _) -> (t, ctx, KUnknown (ctx, cont))
  | (Err t, _) -> (t, ctx, KErr (ctx, cont))
  | (Cast {source; target; term}, _) -> (target, ctx, KCast_target (source, term, ctx, cont))

  | (_, _) -> failwith "stuck term"

(** Transitive clousure of reduce1 with fuel *)
let rec reduce_fueled (fuel : int) ((term, _, cont) as s) : vterm =
  if fuel < 0 || (is_value term && cont == KHole)
     then term
     else reduce_fueled (fuel-1) (reduce1 s)

(** Reduces a term *)
let reduce (t : term): term = 
  to_vterm t |> 
  initial_state |>
  reduce_fueled 1000 |>
  from_vterm