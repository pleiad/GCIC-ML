open Vernac
open Common.Id

type parsed_term = Parsing.Ast.term
type term = Kernel.Ast.term

let from_opt_name id = Option.value id ~default:Name.default

let rec of_parsed_term (t : parsed_term) : term =
  match t with
  | Var x -> Var x
  | Universe i -> Universe i
  | App (t, u) -> App (of_parsed_term t, of_parsed_term u)
  | Lambda (args, body) -> List.fold_right expand_lambda args (of_parsed_term body)
  | Prod (args, body) -> List.fold_right expand_prod args (of_parsed_term body)
  | Unknown i -> Unknown i
  | LetIn (id, ty, t1, t2) ->
    let f = Kernel.Ast.Lambda { id; dom = of_parsed_term ty; body = of_parsed_term t2 } in
    App (f, of_parsed_term t1)
  | Ascription (t, ty) -> Ascription (of_parsed_term t, of_parsed_term ty)

and expand_lambda (id, dom) body =
  Lambda { id = from_opt_name id; dom = of_parsed_term dom; body }

and expand_prod (id, dom) body =
  Prod { id = from_opt_name id; dom = of_parsed_term dom; body }

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

(** Compiles a string and returns the stringified version of the AST *)
let compile (line : string) =
  let open Vernac.Exec in
  match Parsing.Lex_and_parse.parse_command line with
  | Ok cmd ->
    of_parsed_command cmd
    |> execute
    |> Result.fold ~ok:string_of_cmd_result ~error:string_of_error
  | Error e -> e
