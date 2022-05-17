(** This module specifies the operational semantics *)

open Ast

type reduction_error = [ `Err_not_enough_fuel | `Err_stuck_term of term ]

val string_of_error : reduction_error -> string

val reduce : term -> (term, [> reduction_error ]) result
(** Reduces a term *)

val step : term -> (term, [> `Err_stuck_term of term]) result
(** One step reduction *)
