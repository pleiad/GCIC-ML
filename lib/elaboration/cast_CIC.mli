open Main
open Common.Id
open Common

type elaboration_error =
  [ `Err_free_identifier of Name.t
  | `Err_inconsistent of GCIC.term * CastCIC.term * CastCIC.term
  | `Err_constrained_universe of GCIC.term
  | `Err_constrained_product of GCIC.term
  | `Err_constrained_inductive of GCIC.term
  ]

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of CastCIC.term
  | `Err_free_const
  ]

type errors =
  [ reduction_error
  | elaboration_error
  ]

module type Reducer = sig
  val reduce : CastCIC.term -> (CastCIC.term, [> reduction_error ]) result
end

module type Store = sig
  type ind_info =
    { params : (Name.t * CastCIC.term) list
    ; level : int
    }

  type ctor_info =
    { params : (Name.t * CastCIC.term) list
    ; args : (Name.t * CastCIC.term) list
    ; ind : Name.t
    }

  val find_const : Name.t -> CastCIC.term
  val find_ind : Name.t -> ind_info
  val find_ctor_info : Name.t -> ctor_info
end

module type Typer = sig
  type t
  type i
  type c

  val check_type : t Name.Map.t -> t -> t -> c
  val infer_type : t Name.Map.t -> t -> i
end

module type CastCICElab =
  Elaboration with type o = (CastCIC.term * CastCIC.term, errors) result

module Make (ST : Store) (R : Reducer) : CastCICElab
