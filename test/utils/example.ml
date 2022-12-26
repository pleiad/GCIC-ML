open Common.CastCIC
open Common.Id

let reduce = Gcic.CastCIC.CastCICReduction.reduce
let empty_ctx = Name.Map.empty
let name_of_int n = string_of_int n |> Name.of_string

let id_of x = Name.of_string x
let id = Name.of_string "__"
let idx = Name.of_string "x"
let idy = Name.of_string "y"

let idf = Lambda { id; dom = Universe 0; body = Var id }
let unknown i = Unknown (Universe i)

let germ_prod (i : int) : term =
  let cprod = Config.cast_universe_level i in
  let univ = Universe cprod in
  if cprod >= 0
  then Prod { id = Name.default; dom = Unknown univ; body = Unknown univ }
  else Err univ

let germ_univ (i : int) (j : int) : term = if j < i then Universe j else Err (Universe i)

(* From the GCIC paper, this is the elaboration of delta (from which omega is built) *)
let delta' i =
  let dom =
    Cast
      { source = unknown (i + 1); target = Universe i; term = Unknown (unknown (i + 1)) }
  in
  Lambda
    { id
    ; dom
    ; body =
        App
          ( Cast { source = dom; target = germ_prod i; term = Var id }
          , Cast
              { source = dom
              ; target = unknown (Config.cast_universe_level i)
              ; term = Var id
              } )
    }

let omega i =
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
