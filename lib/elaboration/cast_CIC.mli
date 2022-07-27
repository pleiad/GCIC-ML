open Main
open Ast
open Common.Id
open Common

type elaboration_error =
  [ `Err_free_identifier of Name.t
  | `Err_inconsistent of GCIC.term * elaborated_term * elaborated_term
  | `Err_constrained_universe of GCIC.term
  | `Err_constrained_product of GCIC.term
  | `Err_constrained_inductive of GCIC.term
  ]

type reduction_error =
  [ `Err_not_enough_fuel
  | `Err_stuck_term of elaborated_term
  | `Err_free_const
  ]

type errors =
  [ reduction_error
  | elaboration_error
  ]

module type Reducer = sig
  val reduce : elaborated_term -> (elaborated_term, [> reduction_error ]) result
end

module type Store = sig
  type ind_info =
    { params : (Name.t * elaborated_term) list
    ; level : int
    }

  type ctor_info =
    { params : (Name.t * elaborated_term) list
    ; args : (Name.t * elaborated_term) list
    ; ind : Name.t
    }

  val find_const : Name.t -> elaborated_term
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
  Elaboration with type o = (elaborated_term * elaborated_term, errors) result

module Make (ST : Store) (R : Reducer) : CastCICElab
