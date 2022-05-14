open Format

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let (lnum, cnum) = Sedlexing.loc lexbuf in
  asprintf "Line:%d Position:%d" lnum cnum

let parse_term lexbuf =
  try 
    let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let program = MenhirLib.Convert.Simplified.traditional2revised Parser.program lexer in
    Ok program 
  with
  (* catch exception and turn into Error *)
  | Lexer.SyntaxError msg ->
      let error_msg = asprintf "%s: %s@." (print_error_position lexbuf) msg in
      Error error_msg
  | Parser.Error ->
      let error_msg = asprintf "%s: syntax error@." (print_error_position lexbuf) in
      Error error_msg

let term_of_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  parse_term lexbuf