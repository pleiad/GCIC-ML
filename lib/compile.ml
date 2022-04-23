open Parsing
(* open Lexer *)

(** Compiles a string and returns the stringified version of the AST *)
let compile (line : string) =
  let linebuf = Lexing.from_string line in
  match Lex_and_parse.parse_term linebuf with
  | Ok term -> Ast.to_string term
  | Error e -> e

