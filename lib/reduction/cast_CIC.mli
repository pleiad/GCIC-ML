open Main
open Common.CastCIC
open Common.Id

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of term
  | `Err_free_const
  ]

module type Store = sig
  val ind_params : Name.t -> term list
  val const : Name.t -> term
  val ctor_param_args : Name.t -> (Name.t * term) list * (Name.t * term) list
end

module type CastCICRed =
  Reduction with type t = term with type o = (term, reduction_error) result

module Make (ST : Store) : CastCICRed
