open Lexer
open Lexing
open Format

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  asprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_term lexbuf =
  try Ok (Parser.program Lexer.read lexbuf) with
  (* catch exception and turn into Error *)
  | SyntaxError msg ->
      let error_msg = asprintf "%s: %s@." (print_error_position lexbuf) msg in
      Error error_msg
  | Parser.Error ->
      let error_msg = asprintf "%s: syntax error@." (print_error_position lexbuf) in
      Error error_msg

let term_of_string str =
  let linebuf = Lexing.from_string str in
  match parse_term linebuf with
  | Ok term -> term
  | Error _ -> failwith "Cannot parse"