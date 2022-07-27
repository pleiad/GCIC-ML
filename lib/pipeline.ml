open Common.Id
open Common.Declarations
open Common

type elaborated_term = CastCIC.term

module Const = Make_Store (struct
  type t = GCIC.term const_decl
  type cached_t = elaborated_term const_decl
end)

module Ind = Make_Store (struct
  type t = GCIC.term ind_decl
  type cached_t = elaborated_term ind_decl
end)

module Ctor = Make_Store (struct
  type t = GCIC.term ctor_decl
  type cached_t = elaborated_term ctor_decl
end)

module ReductionStore : Reduction.Cast_CIC.Store = struct
  let ind_params i =
    let ind = Ind.find i in
    ind.params |> List.map snd

  let const c = (Const.find c).term

  let ctor_param_args c =
    let ctor = Ctor.find c in
    ctor.params, ctor.args
end

module CastCICReduction = Reduction.Cast_CIC.Make (ReductionStore)

module TypingReducer : Typing.Cast_CIC.Reducer = struct
  type error =
    [ `Err_not_enough_fuel
    | `Err_stuck_term of elaborated_term
    | `Err_free_const
    ]

  let translate_error (e : Reduction.Cast_CIC.reduction_error) : [> error ] =
    match e with
    | `Err_not_enough_fuel -> `Err_not_enough_fuel
    | `Err_stuck_term t -> `Err_stuck_term t
    | `Err_free_const -> `Err_free_const

  let reduce t = CastCICReduction.reduce t |> Result.map_error translate_error
end

module TypingStore : Typing.Cast_CIC.Store = struct
  type ctor_info =
    { params : (Name.t * elaborated_term) list
    ; args : (Name.t * elaborated_term) list
    ; ind : Name.t
    }

  let find_const c = (Const.find c).ty
  let find_ind_params i = (Ind.find i).params

  let find_ctor_info c =
    let ctor = Ctor.find c in
    { params = ctor.params; args = ctor.args; ind = ctor.ind }
end

module CastCICTyping = Typing.Cast_CIC.Make (TypingStore) (TypingReducer)

module ElabStore : Elaboration.Cast_CIC.Store = struct
  type ind_info =
    { params : (Name.t * elaborated_term) list
    ; level : int
    }

  type ctor_info =
    { params : (Name.t * elaborated_term) list
    ; args : (Name.t * elaborated_term) list
    ; ind : Name.t
    }

  let find_const c : elaborated_term = (Const.find c).ty

  let find_ind ind : ind_info =
    let ind_info = Ind.find ind in
    { params = ind_info.params; level = ind_info.level }

  let find_ctor_info c : ctor_info =
    let ctor = Ctor.find c in
    { params = ctor.params; args = ctor.args; ind = ctor.ind }
end

module ElabReducer : Elaboration.Cast_CIC.Reducer = struct
  let translate_error (e : Reduction.Cast_CIC.reduction_error)
      : [> Elaboration.Cast_CIC.reduction_error ]
    =
    match e with
    | `Err_not_enough_fuel -> `Err_not_enough_fuel
    | `Err_stuck_term t -> `Err_stuck_term t
    | `Err_free_const -> `Err_free_const

  let reduce t = CastCICReduction.reduce t |> Result.map_error translate_error
end

module CastCICElab = Elaboration.Cast_CIC.Make (ElabStore) (TypingReducer)
