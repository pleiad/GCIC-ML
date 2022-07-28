open Common.CastCIC
open Common.Id

type type_error =
  [ `Err_not_convertible of term * term
  | `Err_free_identifier of Name.t
  | `Err_not_product of term * term
  | `Err_not_universe of term * term
  | `Err_not_inductive of term * term
  ]

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of term
  | `Err_free_const
  ]

type errors =
  [ reduction_error
  | type_error
  ]

(** Extracts the error message or description *)
val string_of_error : errors -> string

module type Reducer = sig
  val reduce : term -> (term, errors) result
end

module type Store = sig
  type ctor_info =
    { params : (Name.t * term) list
    ; args : (Name.t * term) list
    ; ind : Name.t
    }

  val find_const : Name.t -> term
  val find_ind_params : Name.t -> (Name.t * term) list
  val find_ctor_info : Name.t -> ctor_info
end

module type CastCICTyping =
  Main.Typing
    with type t = term
    with type i = (term, errors) result
    with type c = (unit, errors) result

module Make (ST : Store) (R : Reducer) : CastCICTyping
