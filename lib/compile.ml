open Parsing
open Common


let from_opt_name id = Option.value id ~default:Id.Name.default

let rec of_parsed_term (t : Parsing.Ast.term) : Kernel.Ast.term =
  match t with 
  | Var x -> Var x 
  | Universe i -> Universe i 
  | App (t, u) -> App (of_parsed_term t, of_parsed_term u)
  | Lambda (args, body) -> List.fold_right expand_lambda args (of_parsed_term body)
  | Prod (args, body) -> List.fold_right expand_prod args (of_parsed_term body)
  | Unknown i -> Unknown i
and expand_lambda (id, dom) body = Lambda {id=from_opt_name id; dom=of_parsed_term dom; body}
and expand_prod (id, dom) body = Prod {id=from_opt_name id; dom=of_parsed_term dom; body}

(** Compiles a string and returns the stringified version of the AST *)
let compile (line : string) =
  let linebuf = Sedlexing.Utf8.from_string line in
  match Lex_and_parse.parse_term linebuf with
  | Ok term -> 
    let open Cast_cic in
    (match of_parsed_term term |> Elaboration.elaborate Context.empty with 
    | Ok (elab, _) -> Reduction.reduce elab |> Ast.to_string 
    | Error e -> Elaboration.string_of_error e)    
  | Error e -> e

