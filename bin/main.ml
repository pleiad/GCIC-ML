open Stdio

let get_input () =
  print_string "> ";
  flush Out_channel.stdout;
  In_channel.input_line In_channel.stdin

let rec eval = function
  | None -> ()
  | Some x ->
    print_endline (Gcic.Main.run x);
    eval (get_input ())

let () = eval (get_input ())
