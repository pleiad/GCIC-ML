(** This module executes the lexer and parser. It acts as an interface between the parsing
    code and the main method (abstracting away the underlying implementation) *)

(** Given a lex buffer to read a GCIC term from, parse the
term and return the AST if successful *)
val parse_term : string -> (Ast.term, string) result

val term_of_string : string -> (Ast.term, string) result