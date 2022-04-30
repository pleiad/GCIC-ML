(** This module specifies the operational semantics *)

open Cast_cic

(** Reduces a term *)
val reduce : term -> term