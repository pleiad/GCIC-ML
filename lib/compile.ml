open Vernac
open Common.Id

type parsed_term = Parsing.Ast.term
type term = Kernel.Ast.term

let from_opt_name id = Option.value id ~default:Name.default

(* For simplicity, any free identifier is treated as a Const (an identifier refering to a global declaration).
   A better approach would be to have a list of global declarations and treat free identifiers as just free.  *)
let rec of_parsed_term (t : parsed_term) : term =
  match t with
  | Var x -> if Cast_cic.Declarations.exists x then Const x else Var x
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

let of_parsed_gdef
    : parsed_term Command.global_definition -> term Command.global_definition
  = function
  | Constant_def { name; ty; term } ->
    Constant_def { name; ty = of_parsed_term ty; term = of_parsed_term term }

let of_parsed_command : parsed_term Command.t -> term Command.t = function
  | Eval t -> Eval (of_parsed_term t)
  | Check t -> Check (of_parsed_term t)
  | Elab t -> Elab (of_parsed_term t)
  | Set cfg -> Set cfg
  | Define gdef -> Define (of_parsed_gdef gdef)
  | Load filename -> Load filename

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
