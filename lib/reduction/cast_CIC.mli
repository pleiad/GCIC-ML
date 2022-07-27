open Main
open Common.CastCIC
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

module type CastCICRed =
  Reduction with type t = term with type o = (term, reduction_error) result

module Make (ST : Store with type t = term) : CastCICRed
