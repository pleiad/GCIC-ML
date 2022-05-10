(** This module specifies the operational semantics *)

open Ast

val reduce_in : context -> term -> term
(** Reduces a term in the given context *)

val reduce : term -> term
(** Reduces a term *)

val step : context -> term -> (term, string) result
(** One step reduction *)
