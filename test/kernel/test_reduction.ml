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

(* TODO: Check weights and distribution in subterms *)
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

(* TODO: Add shrinker *)
let arbitrary_cast_cic = QCheck.make cast_cic_gen ~print:to_string

let reduction_to_canonical =
  QCheck.(
    Test.make ~count:1000 ~name:"well-typed terms reduce to canonical"
      arbitrary_cast_cic (fun t ->
        assume (Typing.infer_type Context.empty t |> Result.is_ok);
        Reduction.reduce t |> is_canonical))

(* TODO: Improve this with proper testable  *)
let test_unknown_reduce () =
  Alcotest.(check bool)
    "reduce prod unknown to lambda" true
    (match
       Reduction.reduce
         (Unknown
            (Prod { id = name_of_int 1; dom = Universe 5; body = Universe 0 }))
     with
    | Lambda { id = _; dom = Universe 5; body = Unknown (Universe 0) } -> true
    | _ -> false)

let test_error_reduce () =
  Alcotest.(check bool)
    "reduce prod error to lambda" true
    (match
       Reduction.reduce
         (Err (Prod { id = name_of_int 1; dom = Universe 5; body = Universe 0 }))
     with
    | Lambda { id = _; dom = Universe 5; body = Err (Universe 0) } -> true
    | _ -> false)

let tests =
  [
    ("reduce unknown", `Quick, test_unknown_reduce);
    ("reduce error", `Quick, test_error_reduce);
    QCheck_alcotest.to_alcotest reduction_to_canonical;
  ]
