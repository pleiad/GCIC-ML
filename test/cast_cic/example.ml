open Cast_cic
open Common.Id

let empty_ctx = Name.Map.empty
let name_of_int n = string_of_int n |> Name.of_string
let id = Name.of_string "__"

let idf =
  let open Ast in
  Lambda { id; dom = Universe 0; body = Var id }

let unknown i = Ast.Unknown (Ast.Universe i)

(* From the GCIC paper, this is the elaboration of delta (from which omega is built) *)
let delta' i =
  let open Ast in
  let open Reduction in
  let dom =
    Cast
      { source = unknown (i + 1); target = Universe i; term = Unknown (unknown (i + 1)) }
  in
  Lambda
    { id
    ; dom
    ; body =
        App
          ( Cast { source = dom; target = germ i HProd; term = Var id }
          , Cast
              { source = dom
              ; target = unknown (Config.cast_universe_level i)
              ; term = Var id
              } )
    }

let omega i =
  let open Ast in
  let d' = delta' i in
  let dom =
    Cast
      { source = Unknown (Universe (i + 1))
      ; target = Universe i
      ; term = Unknown (Unknown (Universe (i + 1)))
      }
  in
  App
    ( d'
    , Cast
        { source = Prod { id; dom; body = unknown (Config.cast_universe_level i) }
        ; target = dom
        ; term = d'
        } )
