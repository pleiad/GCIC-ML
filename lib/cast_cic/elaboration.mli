(** This module specifies the elaboration from GCIC to CastCIC *)

(* TODO: This module should go somewhere else, but idk where *)

open Common
open Reduction

(** An error originating from elaboration  *)
type elaboration_error =
  [ `Err_free_identifier of Common.Id.Name.t
  | `Err_inconsistent of Kernel.Ast.term * Ast.term * Ast.term
  | `Err_constrained_universe of Kernel.Ast.term
  | `Err_constrained_product of Kernel.Ast.term
  | `Err_impossible of Kernel.Ast.term
  ]

(** Returns a string representation of the error *)
val string_of_error : elaboration_error -> string

(** Type alias for the elaboration result  *)
type elaboration = Ast.term * Ast.term

(** The elaboration procedure, as per the paper *)
val elaborate
  :  Ast.term Context.t
  -> Kernel.Ast.term
  -> (elaboration, [> elaboration_error | reduction_error ]) result

val check_elab
  :  Ast.term Context.t
  -> Kernel.Ast.term
  -> Ast.term
  -> (Ast.term, [> elaboration_error | reduction_error ]) result

val elab_univ
  :  Ast.term Context.t
  -> Kernel.Ast.term
  -> (Ast.term * int, [> elaboration_error | reduction_error ]) result
