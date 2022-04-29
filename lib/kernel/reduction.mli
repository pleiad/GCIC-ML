(** This module specifies the operational semantics *)

open Cast_cic

(** Checks if a term is in neutral form *)
val is_neutral : term -> bool 

(** Checks if a term is in canonical form *)
val is_canonical : term -> bool 

(** One step reduction of terms *)
val reduce1 : term -> term

(** Transitive clousure of reduce1 *)
val reduce : term -> term