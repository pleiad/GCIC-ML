open Kernel
open Arbitrary

let passing =
  QCheck.(
    Test.make ~count:100 ~name:"Reduction" arbitrary_cast_cic_term (fun t ->
        assume (Typing.infer_type Context.empty t |> Result.is_ok);
        Reduction.reduce t |> Cast_cic.is_canonical))

let tests = List.map QCheck_alcotest.to_alcotest [ passing ]