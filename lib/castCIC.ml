open Common.Id
open Common.Declarations
open Common
open Common.Std

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
  let translate_error (e : Reduction.Cast_CIC.reduction_error) : Typing.Cast_CIC.errors =
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

let translate_error (e : Reduction.Cast_CIC.reduction_error)
    : [> Elaboration.Cast_CIC.reduction_error ]
  =
  match e with
  | `Err_not_enough_fuel -> `Err_not_enough_fuel
  | `Err_stuck_term t -> `Err_stuck_term t
  | `Err_free_const -> `Err_free_const

module ElabReducer : Elaboration.Cast_CIC.Reducer = struct
  let reduce t = CastCICReduction.reduce t |> Result.map_error translate_error
end

module CastCICElab = Elaboration.Cast_CIC.Make (ElabStore) (ElabReducer)

module Executor : Main.Executor = struct
  type o = CastCIC.term

  type cmd_result =
    | Reduction of o
    | Unit
    | Elaboration of o
    | Inference of o
    | Definition of Common.Id.Name.t * Common.GCIC.term

  let string_of_cmd_result : cmd_result -> string = function
    | Reduction t -> CastCIC.to_string t
    | Unit -> "OK"
    | Elaboration t -> CastCIC.to_string t
    | Inference t -> CastCIC.to_string t
    | Definition (name, ty) ->
      Name.to_string name ^ " : " ^ GCIC.to_string ty ^ " defined."

  type execute_error = string
  (* [ Elaboration.Cast_CIC.elaboration_error
    | Elaboration.Cast_CIC.reduction_error
    | Typing.Cast_CIC.type_error
    | Reduction.Cast_CIC.reduction_error
    | `LoadError of execute_error
    | `FileNotFound of string
    ] *)

  let string_of_error str = str
  (* function
    | #Elaboration.Cast_CIC.elaboration_error as e ->
      "[elaboration_error] " ^ Elaboration.Cast_CIC.string_of_error e
    | #Typing.Cast_CIC.type_error as e ->
      "[type_error] " ^ Typing.Cast_CIC.string_of_error e
    | #Reduction.Cast_CIC.reduction_error as e ->
      "[reduction_error] " ^ Reduction.Cast_CIC.string_of_error e
    | `LoadError e -> "[load_error]\n" ^ string_of_error e
    | `FileNotFound filename -> "[file_not_found_error] " ^ filename *)

  let execute_eval term : (cmd_result, execute_error) result =
    let* elab_term, _ =
      CastCICElab.elaborate term |> Result.map_error Elaboration.Cast_CIC.string_of_error
    in
    let* v =
      CastCICReduction.reduce elab_term
      |> Result.map_error Reduction.Cast_CIC.string_of_error
    in
    Ok (Reduction v)

  let execute_check term : (cmd_result, execute_error) result =
    let empty_ctx = Name.Map.empty in
    let* (elab_term, _) : elaborated_term * elaborated_term =
      CastCICElab.elaborate term |> Result.map_error Elaboration.Cast_CIC.string_of_error
    in
    let* ty =
      CastCICTyping.infer_type empty_ctx elab_term
      |> Result.map_error Typing.Cast_CIC.string_of_error
    in
    let* v =
      CastCICReduction.reduce ty |> Result.map_error Reduction.Cast_CIC.string_of_error
    in
    (* By default it normalizes the output. TODO: Add flag *)
    Ok (Inference v)

  let execute_elab term : (cmd_result, execute_error) result =
    let* elab_term, _ =
      CastCICElab.elaborate term |> Result.map_error Elaboration.Cast_CIC.string_of_error
    in
    Ok (Elaboration elab_term)

  let execute_set_flag (flag : Config.Flag.t) : (cmd_result, execute_error) result =
    Config.set_flag flag;
    Ok Unit

  let execute_definition gdef : (cmd_result, execute_error) result =
    let empty_ctx = Name.Map.empty in
    match gdef with
    | { name; ty; term } ->
      let* elab_ty, _ =
        CastCICElab.elab_univ empty_ctx ty
        |> Result.map_error Elaboration.Cast_CIC.string_of_error
      in
      let* elab_term =
        CastCICElab.check_elab empty_ctx term elab_ty
        |> Result.map_error Elaboration.Cast_CIC.string_of_error
      in
      let* _ =
        CastCICTyping.check_type empty_ctx elab_term elab_ty
        |> Result.map_error Typing.Cast_CIC.string_of_error
      in
      Const.add name { name; ty; term };
      Const.add_cache name { name; ty = elab_ty; term = elab_term };
      Ok (Definition (name, ty))

  let execute_inductive ind ctors : (cmd_result, execute_error) result =
    let empty_ctx = Name.Map.empty in
    let elab_univ_param (elab_params, ctx) (id, param) =
      let* elab_param, _ =
        CastCICElab.elab_univ ctx param
        |> Result.map_error Elaboration.Cast_CIC.string_of_error
      in
      Ok ((id, elab_param) :: elab_params, Name.Map.add id elab_param ctx)
    in
    let elab_univ_params ctx params =
      let* elab_params, elab_ctx = fold_results elab_univ_param (Ok ([], ctx)) params in
      Ok (List.rev elab_params, elab_ctx)
    in
    let execute_ind_decl (ind : Ind.t) =
      let* elab_sort, level =
        CastCICElab.elab_univ empty_ctx ind.sort
        |> Result.map_error Elaboration.Cast_CIC.string_of_error
      in
      let* elab_params, _ = elab_univ_params empty_ctx ind.params in
      let cached_ind =
        { ind with sort = elab_sort; level = level - 1; params = elab_params }
      in
      (* TODO: Missing uniqueness of name (no more than one inductive with a given name) *)
      Ind.add ind.name ind;
      Ind.add_cache ind.name cached_ind;
      string_of_cmd_result (Definition (ind.name, ind.sort)) |> print_endline;
      Ok Unit
    in
    let execute_ctor_decl (ctor : Ctor.t) =
      let* elab_params, elab_ctx = elab_univ_params empty_ctx ctor.params in
      let* elab_args, _ = elab_univ_params elab_ctx ctor.args in
      let* elab_ty, _ =
        CastCICElab.elab_univ empty_ctx ctor.ty
        |> Result.map_error Elaboration.Cast_CIC.string_of_error
      in
      (* FIXME: Check whether the type of the constructor matches the inductive's.
       e.g. You can define "cons : a -> list a -> bool" and it will be ok   
    *)
      let cached_ctor =
        { ctor with args = elab_args; params = elab_params; ty = elab_ty }
      in
      (* TODO: Missing uniqueness of constructor in inductive (no more than one constructor with a given name) *)
      Ctor.add ctor.name ctor;
      Ctor.add_cache ctor.name cached_ctor;
      string_of_cmd_result (Definition (ctor.name, ctor.ty)) |> print_endline;
      Ok Unit
    in
    (* FIXME: Not transactional *)
    let* _ = execute_ind_decl ind in
    let* _ = map_results execute_ctor_decl ctors in
    Ok Unit

  exception LoadFail of execute_error

  let rec execute file_parser cmd : (cmd_result, execute_error) result =
    let open Parsing.Command in
    match cmd with
    | Eval t -> execute_eval t
    | Check t -> execute_check t
    | Elab t -> execute_elab t
    | Set f -> execute_set_flag f
    | Define gdef -> execute_definition gdef
    | Load filename -> execute_load file_parser filename
    | Inductive (ind, ctors) -> execute_inductive ind ctors

  and execute_load file_parser filename =
    try
      let src = Stdio.In_channel.read_all filename in
      let cmds = file_parser src in
      List.fold_left
        (fun _ cmd ->
          match execute file_parser cmd with
          | Ok res -> string_of_cmd_result res |> print_endline
          | Error e -> raise (LoadFail e))
        ()
        cmds;
      Ok Unit
    with
    | LoadFail e -> Error ("[load_error]\n" ^ string_of_error e)
    | Sys_error _ -> Error ("[file_not_found_error] " ^ filename)
end
