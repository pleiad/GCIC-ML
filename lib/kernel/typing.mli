(** This module specifies the typing relation *)

open Cast_cic

type type_error
val error_msg : type_error -> string

val infering : context -> term -> (term, type_error) result
val checking : context -> term -> term -> (unit, type_error) result
val infering_prod : context -> term -> (Name.t * term * term, type_error) result
val infering_univ : context -> term -> (int, type_error) result