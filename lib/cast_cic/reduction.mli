(** This module specifies the operational semantics *)

open Ast

val reduce : term -> term
(** Reduces a term *)

val step : term -> (term, string) result
(** One step reduction *)
