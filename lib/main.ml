open Common.Id
open Common.Std
open Common.Declarations

module type Run = sig
  val run : string -> string
end

module type Executor = sig
  type o

  type cmd_result =
    | Reduction of o
    | Unit
    | Elaboration of o
    | Inference of o
    | Definition of Common.Id.Name.t * Common.GCIC.term

  (** Returns the stringified version of the command's result *)
  val string_of_cmd_result : cmd_result -> string

  (** The type of errors occurring during the execution of commands *)
  type execute_error

  (** Returns the stringified version of an error *)
  val string_of_error : execute_error -> string

  (** Executes a command *)
  val execute
    :  (string -> Common.GCIC.term Parsing.Command.t list)
    -> Common.GCIC.term Parsing.Command.t
    -> (cmd_result, execute_error) result
end

module Make (E : Executor) : Run = struct
  type parsed_term = Parsing.Ast.term
  type term = Common.GCIC.term

  let defined_ids : [ `Const | `Ind of int * int | `Ctor of int ] Name.Map.t ref =
    ref Name.Map.empty

  let from_opt_name id = Option.value id ~default:Name.default

  (* For simplicity, any free identifier is treated as a Const (an identifier refering to a global declaration).
     A better approach would be to have a list of global declarations and treat free identifiers as just free.  *)
  let rec of_parsed_term (t : parsed_term) : term =
    match t with
    | Var x ->
      (try
         match Name.Map.find x !defined_ids with
         | `Const -> Const x
         | `Ind (lvl, _) -> Inductive (x, lvl, [])
         | `Ctor _ -> Constructor (x, [])
       with
      | Not_found -> Var x)
    | Universe i -> Universe i
    | App (t, u) ->
      let t' = of_parsed_term t in
      let u' = of_parsed_term u in
      (* Checks if a constructor or inductive can be applied to more arguments. *)
      let is_not_saturated (x : Name.t) (actual : int) : bool =
        try
          match Name.Map.find x !defined_ids with
          | `Const -> false
          | `Ind (_, expected) | `Ctor expected -> actual < expected
        with
        | Not_found -> false
      in
      (match t' with
      | Inductive (ind, i, params) when is_not_saturated ind (List.length params) ->
        Inductive (ind, i, List.append params [ u' ])
      | Constructor (ctor, args) when is_not_saturated ctor (List.length args) ->
        Constructor (ctor, List.append args [ u' ])
      | _ -> App (t', u'))
    | Lambda (args, body) -> expand_lambda (fun fi -> Common.GCIC.Lambda fi) args body
    | Prod (args, body) -> expand_lambda (fun fi -> Common.GCIC.Prod fi) args body
    | Unknown i -> Unknown i
    | Match mi ->
      let discr = of_parsed_term mi.discr in
      let pred = of_parsed_term mi.pred in
      let of_parsed_branch (branch : Parsing.Ast.branch) : Common.GCIC.branch =
        (* FIXME: These IDs can possibly not include the inductive's parameters, 
        which results in an elaboration error. For instance:
        | nil -> <term> 
        will store no params nor args, and then at elaboration it will try to 
        check that the parameters and args match the expected one.
        The fix for now is to be explicit in the branches as well, e.g. 
        | nil bool -> true
        *)
        { ctor = branch.ctor; ids = branch.ids; term = of_parsed_term branch.body }
      in
      let branches = List.map of_parsed_branch mi.branches in
      Match
        { discr
        ; pred
        ; branches
        ; ind = mi.ind
        ; z = mi.z
        ; f = Name.of_string ("rec_" ^ Name.to_string mi.ind)
        }
    | LetIn (id, ty, t1, t2) ->
      let f = Parsing.Ast.Lambda ([ Some id, ty ], t2) in
      of_parsed_term (App (f, t1))
    | Ascription (t, ty) -> Ascription (of_parsed_term t, of_parsed_term ty)
    | UnknownT i -> UnknownT i

  and expand_lambda (hd : Common.GCIC.fun_info -> term) args body =
    match args with
    | [] -> of_parsed_term body
    | (id, dom) :: args ->
      hd
        { id = from_opt_name id
        ; dom = of_parsed_term dom
        ; body = expand_lambda hd args body
        }

  let of_parsed_const_decl (d : parsed_term const_decl) =
    defined_ids := Name.Map.add d.name `Const !defined_ids;
    { d with ty = of_parsed_term d.ty; term = of_parsed_term d.term }

  let of_parsed_ind_decl (d : parsed_term ind_decl) =
    let params = List.map (map_snd of_parsed_term) d.params in
    let lvl = of_parsed_term d.sort |> Common.GCIC.get_universe_lvl in
    defined_ids := Name.Map.add d.name (`Ind (lvl, List.length params)) !defined_ids;
    { d with params; sort = of_parsed_term d.sort }

  let of_parsed_ctor_decl (d : parsed_term ctor_decl) =
    let params = List.map (map_snd of_parsed_term) d.params in
    let args = List.map (map_snd of_parsed_term) d.args in
    let arity = List.(length params + length args) in
    let ty = of_parsed_term d.ty in
    defined_ids := Name.Map.add d.name (`Ctor arity) !defined_ids;
    { d with params; args; ty }

  let of_parsed_command : parsed_term Parsing.Command.t -> term Parsing.Command.t
    = function
    | Eval t -> Eval (of_parsed_term t)
    | Check t -> Check (of_parsed_term t)
    | Elab t -> Elab (of_parsed_term t)
    | Set cfg -> Set cfg
    | Define d -> Define (of_parsed_const_decl d)
    | Fix d -> Fix (of_parsed_const_decl d)
    | Load filename -> Load filename
    | Inductive (ind, ctors) ->
      let ind' = of_parsed_ind_decl ind in
      let ctors' = List.map of_parsed_ctor_decl ctors in
      Inductive (ind', ctors')

  let parse_file_content str =
    match Parsing.Lex_and_parse.parse_program str with
    | Ok cmds -> List.map of_parsed_command cmds
    | Error e ->
      print_endline e;
      []

  let execute cmd = of_parsed_command cmd |> E.execute parse_file_content

  (** Compiles a string and returns the stringified version of the AST *)
  let run (line : string) =
    match Parsing.Lex_and_parse.parse_command line with
    | Ok cmd ->
      execute cmd |> Result.fold ~ok:E.string_of_cmd_result ~error:E.string_of_error
    | Error e -> e
end
