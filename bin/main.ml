open Gcic
open Stdio

let rec eval = function
  | None -> ()
  | Some x ->
    print_endline (Compile.compile x);
    eval (In_channel.input_line In_channel.stdin)

let () = eval (In_channel.input_line In_channel.stdin)
