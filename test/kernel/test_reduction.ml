open Kernel
open! Kernel.Cast_cic

let name_of_int n = string_of_int n |> Name.of_string
let var n = Var (name_of_int n)
let universe n = Universe n
let app t1 t2 = App (t1, t2)
let lambda i dom body = Lambda { id = name_of_int i; dom; body }
let prod i dom body = Prod { id = name_of_int i; dom; body }
let unknown t = Unknown t
let err t = Err t
let cast source target term = Cast { source; target; term }

let cast_cic_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
           match n with
           | 0 -> map universe nat
           | n ->
               frequency
                 [
                   (1, map var nat);
                   (1, map universe nat);
                   (1, map2 app (self (n / 2)) (self (n / 2)));
                   (1, map3 lambda nat (self (n / 2)) (self (n / 2)));
                   (1, map3 prod nat (self (n / 2)) (self (n / 2)));
                   (1, map unknown (self n));
                   (1, map err (self n));
                   (1, map3 cast (self (n / 3)) (self (n / 3)) (self (n / 3)));
                 ]))

let arbitrary_cast_cic =
  (* let open QCheck.Iter in *)
  (* let print_cast_cic = to_string in *)
  (* let rec shrink_cast_cic = function
       | Var x ->  QCheck.Shrink.int (x |> Name.to_string |> int_of_string) >|= var
       | Universe i -> QCheck.Shrink.int i >|= var
       | App t1 t2 ->
         of_list [ t1; t2 ]
           <+> (shrink_cast_cic t1 >|= fun t1' -> app t1't2)
           <+> (shrink_cast_cic t2 >|= fun t2' -> app t1 t2')
       | _ -> var 1

     in *)
  QCheck.make cast_cic_gen ~print:to_string

let passing =
  QCheck.(
    Test.make ~count:100 ~name:"Reduction" arbitrary_cast_cic (fun t ->
        assume (Typing.infer_type Context.empty t |> Result.is_ok);
        Reduction.reduce t |> is_canonical))

let tests = List.map QCheck_alcotest.to_alcotest [ passing ]