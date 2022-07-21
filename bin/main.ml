let get_input () =
  let open Stdio in
  print_string "> ";
  flush Out_channel.stdout;
  In_channel.input_line In_channel.stdin

let rec eval = function
  | None -> ()
  | Some x ->
    print_endline (Gcic.Main.run x);
    eval (get_input ())

let () = eval (get_input ())
