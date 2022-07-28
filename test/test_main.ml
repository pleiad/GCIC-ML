exception Parse_error of string

module CastCIC = Gcic.Main.Make (Gcic.CastCIC.Executor)

let run = CastCIC.run
let false_ind = "Inductive false : Type@0 :=."
let bool_ind = "Inductive bool : Type := | false : bool | true : bool."

let list_ind =
  "Inductive list (a : Type) : Type :=\n\
  \  | nil : list a\n\
  \  | cons (hd : a) (tl : list a) : list a\n\
  \  ."

let w_ind =
  "Inductive W (a : Type) (b : a -> Type) : Type :=\n\
  \  | sup (x : a) (f : b x -> W a b) : W a b\n\
  \  .\n\
  \  "

let test_inductive_defs () =
  Alcotest.(check string) "false ind" "OK" (run false_ind);
  Alcotest.(check string) "bool ind" "OK" (run bool_ind);
  Alcotest.(check string) "list ind" "OK" (run list_ind);
  Alcotest.(check string) "W ind" "OK" (run w_ind)

let tests = [ "inductive defs", `Quick, test_inductive_defs ]
