(** This module specifies the elaboration from GCIC to CastCIC *)

open Common.Id

(** An error originating from elaboration  *)
type elaboration_error =
  [ `Err_free_identifier of Name.t
  | `Err_inconsistent of Kernel.Ast.term * Ast.term * Ast.term
  | `Err_constrained_universe of Kernel.Ast.term
  | `Err_constrained_product of Kernel.Ast.term
  | `Err_constrained_inductive of Kernel.Ast.term
  ]

(** Returns a string representation of the error *)
val string_of_error : elaboration_error -> string

(** Type alias for the elaboration result  *)
type elaboration = Ast.term * Ast.term

(** The elaboration procedure, as per the paper *)
val elaborate
  :  (Ast.term -> (Ast.term, ([> elaboration_error ] as 'e)) result)
  -> Ast.term Name.Map.t
  -> Kernel.Ast.term
  -> (elaboration, 'e) result

val check_elab
  :  (Ast.term -> (Ast.term, ([> elaboration_error ] as 'e)) result)
  -> Ast.term Name.Map.t
  -> Kernel.Ast.term
  -> Ast.term
  -> (Ast.term, 'e) result

val elab_univ
  :  (Ast.term -> (Ast.term, ([> elaboration_error ] as 'e)) result)
  -> Ast.term Name.Map.t
  -> Kernel.Ast.term
  -> (Ast.term * int, 'e) result
