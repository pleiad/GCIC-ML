open Vernac
open Common.Id
open Common.Std
open Kernel.Declarations

type parsed_term = Parsing.Ast.term
type term = Kernel.Ast.term

let defined_ids : [`Const | `Ind | `Ctor] Name.Map.t ref = ref Name.Map.empty

let from_opt_name id = Option.value id ~default:Name.default

(* For simplicity, any free identifier is treated as a Const (an identifier refering to a global declaration).
   A better approach would be to have a list of global declarations and treat free identifiers as just free.  *)
let rec of_parsed_term (t : parsed_term) : term =
  match t with
  | Var x -> if Name.Map.mem x !defined_ids then Const x else Var x
  | Universe i -> Universe i
  | App (t, u) -> App (of_parsed_term t, of_parsed_term u)
  | Lambda (args, body) -> expand_lambda (fun fi -> Kernel.Ast.Lambda fi) args body
  | Prod (args, body) -> expand_lambda (fun fi -> Kernel.Ast.Prod fi) args body
  | Unknown i -> Unknown i
  | LetIn (id, ty, t1, t2) ->
    let f = Parsing.Ast.Lambda ([ Some id, ty ], t2) in
    of_parsed_term (App (f, t1))
  | Ascription (t, ty) -> Ascription (of_parsed_term t, of_parsed_term ty)
  | UnknownT i -> UnknownT i

and expand_lambda (hd : Kernel.Ast.fun_info -> term) args body =
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
  defined_ids := Name.Map.add d.name `Ind !defined_ids;
  { d with
    params = List.map (map_snd of_parsed_term) d.params
  ; sort = of_parsed_term d.sort
  }

let of_parsed_ctor_decl (d : parsed_term ctor_decl) =
  defined_ids := Name.Map.add d.name `Ctor !defined_ids;
  { d with
    params = List.map (map_snd of_parsed_term) d.params
  ; args = List.map (map_snd of_parsed_term) d.args
  ; ty = of_parsed_term d.ty
  }

let of_parsed_command : parsed_term Command.t -> term Command.t = function
  | Eval t -> Eval (of_parsed_term t)
  | Check t -> Check (of_parsed_term t)
  | Elab t -> Elab (of_parsed_term t)
  | Set cfg -> Set cfg
  | Define d -> Define (of_parsed_const_decl d)
  | Load filename -> Load filename
  | Inductive (ind, ctors) ->
    Inductive (of_parsed_ind_decl ind, List.map of_parsed_ctor_decl ctors)


let parse_file_content str =
  match Parsing.Lex_and_parse.parse_program str with
  | Ok cmds -> List.map of_parsed_command cmds
  | Error e ->
    print_endline e;
    []

(** Compiles a string and returns the stringified version of the AST *)
let compile (line : string) =
  let open Vernac.Exec in
  match Parsing.Lex_and_parse.parse_command line with
  | Ok cmd ->
    of_parsed_command cmd
    |> execute parse_file_content
    |> Result.fold ~ok:string_of_cmd_result ~error:string_of_error
  | Error e -> e
