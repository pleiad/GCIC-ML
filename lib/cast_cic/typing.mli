(** This module specifies the typing relation *)

open Ast
open Common.Id
open Reduction
open Elaboration

type typing_context = Ast.term Name.Map.t

(** A typing error *)
type type_error =
  [ `Err_not_convertible of term * term
  | `Err_free_identifier of Name.t
  | `Err_not_product of term * term
  | `Err_not_universe of term * term
  | `Err_not_inductive of term * term
  ]

(** Extracts the error message or description *)
val string_of_error : type_error -> string

(** Infers the type of the given term *)
val infer_type
  :  typing_context
  -> term
  -> (term, [> type_error | reduction_error | elaboration_error ]) result

(** Checks that the term has the given type *)
val check_type
  :  typing_context
  -> term
  -> term
  -> (unit, [> type_error | reduction_error | elaboration_error ]) result

(** Constrained inference, where the inferred type must be convertible to a product *)
val infer_prod
  :  typing_context
  -> term
  -> (Name.t * term * term, [> type_error | reduction_error | elaboration_error ]) result

(** Constrained inference, where the inferred type must be convertible to a universe *)
val infer_univ
  :  typing_context
  -> term
  -> (int, [> type_error | reduction_error | elaboration_error ]) result
