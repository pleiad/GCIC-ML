(** This module executes the lexer and parser. It acts as an interface between the parsing
    code and the main method (abstracting away the underlying implementation) *)

(** Given a string to read a GCIC term from, parse the
term and return the AST if successful *)
val parse_term : string -> (Ast.term, string) result

(** Given a string to read a command from, parse the
command and return it if successful *)
val parse_command : string -> (Ast.command, string) result