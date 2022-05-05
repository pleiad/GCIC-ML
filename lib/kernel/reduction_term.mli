open Cast_cic

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
val to_vterm : term -> vterm
val to_vfun_info : fun_info -> vfun_info

(** Converts a term of the original AST into a term with tagged values *)
val to_vcontext : context -> vcontext

(** Converts a term with tagged values into a term of the original AST *)
val from_vterm : vterm -> term
val from_vfun_info : vfun_info -> fun_info

(** Performs substitution of a context inside a vterm.
    Values are untagged since terms in the context may not be fully reduced (see rule Prod-Prod in reduce1) *)
val subst : vcontext -> vterm -> vterm

(** Checks if a term corresponds to a type *)
val is_type : vterm -> bool

(** Checks if a term corresponds to a tagged value *)
val is_value : vterm -> bool

(** Untags values *)
val from_value : vterm -> vterm