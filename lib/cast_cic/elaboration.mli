(** This module specifies the elaboration from GCIC to CastCIC *)

(* TODO: This module should go somewhere else, but idk where *)

type elaboration_error
(** An error originating from elaboration  *)

val string_of_error : elaboration_error -> string 
(** Returns a string representation of the error *)

type elaboration = Ast.term * Ast.term 
(** Type alias for the elaboration result  *)

val elaborate : Ast.context -> Kernel.Ast.term -> (elaboration, elaboration_error) result
(** The elaboration procedure, as per the paper *)

