(** This module specifies the typing relation *)

open Ast
open Common.Id
open Reduction
open Context

(** A typing error *)
type type_error =
  [ `Err_not_convertible of term * term
  | `Err_free_identifier of Name.t
  | `Err_not_product of term * term
  | `Err_not_universe of term * term
  ]

(** Extracts the error message or description *)
val string_of_error : type_error -> string

(** Infers the type of the given term *)
val infer_type : context -> term -> (term, [> type_error | reduction_error ]) result

(** Checks that the term has the given type *)
val check_type
  :  context
  -> term
  -> term
  -> (unit, [> type_error | reduction_error ]) result

(** Constrained inference, where the inferred type must be convertible to a product *)
val infer_prod
  :  context
  -> term
  -> (Name.t * term * term, [> type_error | reduction_error ]) result

(** Constrained inference, where the inferred type must be convertible to a universe *)
val infer_univ : context -> term -> (int, [> type_error | reduction_error ]) result
