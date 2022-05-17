(** This module specifies the elaboration from GCIC to CastCIC *)

(* TODO: This module should go somewhere else, but idk where *)

open Reduction

type elaboration_error = [
  | `Err_free_identifier of Common.Id.Name.t
  | `Err_inconsistent of Kernel.Ast.term * Ast.term * Ast.term
  | `Err_constrained_universe of Kernel.Ast.term
  | `Err_constrained_product of Kernel.Ast.term
  | `Err_impossible of Kernel.Ast.term
]
(** An error originating from elaboration  *)

val string_of_error : elaboration_error -> string
(** Returns a string representation of the error *)

type elaboration = Ast.term * Ast.term
(** Type alias for the elaboration result  *)

val elaborate : Context.context -> Kernel.Ast.term -> (elaboration, [> elaboration_error | reduction_error]) result
(** The elaboration procedure, as per the paper *)
