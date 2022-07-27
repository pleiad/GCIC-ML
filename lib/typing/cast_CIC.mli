open Ast
open Common.Id

type type_error =
  [ `Err_not_convertible of term * term
  | `Err_free_identifier of Name.t
  | `Err_not_product of term * term
  | `Err_not_universe of term * term
  | `Err_not_inductive of term * term
  ]

module type Reducer = sig
  type error =
    [ `Err_not_enough_fuel
    | `Err_stuck_term of term
    | `Err_free_const
    ]

  val reduce : term -> (term, [> error ]) result
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

module Typing (ST : Store) (R : Reducer) : Main.Typing
