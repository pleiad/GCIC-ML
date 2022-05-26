open Vernac
open Common.Id

type parsed_term = Parsing.Ast.term
type term = Kernel.Ast.term

let from_opt_name id = Option.value id ~default:Name.default

(* For simplicity, any free identifier is treated as a Const (an identifier refering to a global declaration).
   A better approach would be to have a list of global declarations and treat free identifiers as just free.  *)
let rec of_parsed_term ?(ids : Name.t list = []) (t : parsed_term) : term =
  match t with
  | Var x -> if List.mem x ids then Var x else Const x
  | Universe i -> Universe i
  | App (t, u) -> App (of_parsed_term ~ids t, of_parsed_term ~ids u)
  | Lambda (args, body) -> expand_lambda (fun fi -> Kernel.Ast.Lambda fi) ids args body
  | Prod (args, body) -> expand_lambda (fun fi -> Kernel.Ast.Prod fi) ids args body
  | Unknown i -> Unknown i
  | LetIn (id, ty, t1, t2) ->
    let f = Parsing.Ast.Lambda ([ Some id, ty ], t2) in
    of_parsed_term ~ids (App (f, t1))
  | Ascription (t, ty) -> Ascription (of_parsed_term ~ids t, of_parsed_term ~ids ty)
  | UnknownT i -> UnknownT i

and expand_lambda (hd : Kernel.Ast.fun_info -> term) ids args body =
  match args with
  | [] -> of_parsed_term ~ids body
  | (id, dom) :: args ->
    let new_scope = Option.fold ~none:ids ~some:(fun x -> x :: ids) id in
    hd
      { id = from_opt_name id
      ; dom = of_parsed_term ~ids dom
      ; body = expand_lambda hd new_scope args body
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
  | SetVariant v -> SetVariant v
  | Definition gdef -> Definition (of_parsed_gdef gdef)
  | Import filename -> Import filename

(** Compiles a string and returns the stringified version of the AST *)
let compile (line : string) =
  let open Vernac.Exec in
  match Parsing.Lex_and_parse.parse_command line with
  | Ok cmd ->
    of_parsed_command cmd
    |> execute
    |> Result.fold ~ok:string_of_cmd_result ~error:string_of_error
  | Error e -> e
