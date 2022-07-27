open Main
open Ast
open Common.Id

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of term
  | `Err_free_const
  ]

module type Store = sig
  type t

  val ind_params : Name.t -> t list
  val const : Name.t -> t
  val ctor_param_args : Name.t -> (Name.t * t) list * (Name.t * t) list
end

module Red (ST : Store with type t = term) :
  Reduction with type t = term with type error = reduction_error
