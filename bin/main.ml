open Gcic
open Stdio

let get_input () =
  print_string "> ";
  flush Out_channel.stdout;
  In_channel.input_line In_channel.stdin

let rec eval = function
  | None -> ()
  | Some x ->
    print_endline (Compile.compile x);
    eval (get_input ())

let () = eval (get_input ())
