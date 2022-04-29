(** This module specifies the operational semantics *)

open Cast_cic

(** Checks if a type t makes ?_t or err t canonical *)
let is_unknown_or_error_canonical : term -> bool = function
  | Universe _ | Unknown (Universe _) | Err (Universe _) -> true
  | _ -> false

(** Checks if a term is in neutral form *)
let rec is_neutral : term -> bool = function
  | Var _ -> true
  | App (t, _) | Unknown t | Err t
  | Cast {source=Unknown (Universe _); target=_;      term=t}
  | Cast {source=Universe _;           target=t;      term=_}
  | Cast {source=Prod _;               target=Prod _; term=t}
  | Cast {source=Prod _;               target=t;      term=_}
  | Cast {source=t;                    target=_;      term=_} -> is_neutral t
  | _ -> false

(** Checks if a term is in canonical form *)
let is_canonical : term -> bool = function
  | Universe _ | Lambda _ | Prod _ -> true
  | Unknown t when is_unknown_or_error_canonical t -> true
  | Err t when is_unknown_or_error_canonical t -> true
  | Cast {source=ty; target=Unknown (Universe i); term=_} when is_germ i ty -> true
  | t -> is_neutral t

(** One step reduction of terms *)
let reduce1 (t : term) : term =
  t

(** Transitive clousure of reduce1 *)
let reduce (t : term) : term =
  t