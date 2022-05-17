open Format

(* Taken from https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc-syntax-errors/calc.ml *)
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. *)

let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None -> assert false

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. *)

let succeed v = Ok v

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)

let fail text buffer (checkpoint : _ I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  let msg = asprintf "%s%s%s%!" location indication message in
  Error msg

let parse_term text =
  try 
    let lexbuf = Sedlexing.Utf8.from_string text in
    let supplier = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let buffer, supplier = E.wrap_supplier supplier in
    let start_position = fst (Sedlexing.lexing_positions lexbuf) in
    let checkpoint = Parser.Incremental.term_parser start_position in
    
    I.loop_handle succeed (fail text buffer) supplier checkpoint
  with
  (* catch exception and turn into Error *)
  | Lexer.SyntaxError msg ->
      let error_msg = asprintf "%s" msg in
      Error error_msg

let parse_command text =
  try 
    let lexbuf = Sedlexing.Utf8.from_string text in
    let supplier = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let buffer, supplier = E.wrap_supplier supplier in
    let start_position = fst (Sedlexing.lexing_positions lexbuf) in
    let checkpoint = Parser.Incremental.program_parser start_position in
    
    I.loop_handle succeed (fail text buffer) supplier checkpoint
  with
  (* catch exception and turn into Error *)
  | Lexer.SyntaxError msg ->
      let error_msg = asprintf "%s" msg in
      Error error_msg