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

(** Converts a term of the original AST into a term with tagged values *)
let to_vcontext (ctx : context) : vcontext = 
  Context.to_list ctx |>
  List.map (fun (k,v) -> (k, to_vterm v)) |>
  Context.of_list


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

(** Performs substitution of a context inside a vterm.
    Values are untagged since terms in the context may not be fully reduced (see rule Prod-Prod in reduce1) *)
let rec subst ctx : vterm -> vterm = function
  | Var y -> 
    (match Context.lookup ~key:y ~ctx with
     | None -> Var y
     | Some t -> t)
  | Universe i -> Universe i
  | App (t,u) -> App (subst ctx t, subst ctx u)
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
  | Cast { source; target; term=term' } -> 
    Cast { source = subst ctx source;
           target = subst ctx target;
           term = subst ctx term' }
  | VLambda (fi, ctx') -> 
    let y = new_identifier () in
    let ctx'' = Context.add ~key:fi.id ~value:(Var y) ctx' in
    Lambda { id = y; dom = subst ctx' fi.dom; body = subst ctx'' fi.body }
  | VProd (fi, ctx') -> 
    let y = new_identifier () in
    let ctx'' = Context.add ~key:fi.id ~value:(Var y) ctx' in
    Prod { id = y; dom = subst ctx' fi.dom; body = subst ctx'' fi.body }

(** Checks if a term corresponds to a type *)
let is_type : vterm -> bool = function
| VProd _ | Universe _ -> true
| _ -> false

(** Checks if a term corresponds to a tagged value *)
let is_value = function
| VUnknown (VProd _) | VErr (VProd _) -> false
| Universe _ | VLambda _ | VProd _ | VUnknown _ | VErr _ -> true
| _ -> false

(** Untags values *)
let rec from_value : vterm -> vterm = function
| VLambda (fi, _) -> Lambda {fi with dom=from_value fi.dom}
| VProd (fi, _) -> Prod {fi with dom=from_value fi.dom}
| VUnknown t -> Unknown (from_value t)
| VErr t -> Err (from_value t)
| t -> t