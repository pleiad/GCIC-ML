open Parsing
open Common


let rec of_parsed_term (t : Parsing.Ast.term) : Kernel.Ast.term =
  match t with 
  | Var x -> Var x 
  | Universe i -> Universe i 
  | App (t, u) -> App (of_parsed_term t, of_parsed_term u)
  | Lambda (id, dom, body) -> Lambda {id; dom= of_parsed_term dom; body= of_parsed_term body}
  | Prod (id, dom, body) -> Prod {id; dom= of_parsed_term dom; body= of_parsed_term body}
  | Unknown i -> Unknown i

(** Compiles a string and returns the stringified version of the AST *)
let compile (line : string) =
  let linebuf = Lexing.from_string line in
  match Lex_and_parse.parse_term linebuf with
  | Ok term -> 
    let open Cast_cic in
    (match of_parsed_term term |> Elaboration.elaborate Context.empty with 
    | Ok (elab, _) -> Reduction.reduce elab |> Ast.to_string 
    | Error e -> Elaboration.string_of_error e)    
  | Error e -> e

