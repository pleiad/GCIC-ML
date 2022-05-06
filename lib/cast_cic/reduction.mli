(** This module specifies the operational semantics *)

open Ast

(** Reduces a term in the given context *)
val reduce_in : context -> term -> term

(** Reduces a term *)
val reduce : term -> term

(** One step reduction *)
val step : context -> term -> (term, string) result
