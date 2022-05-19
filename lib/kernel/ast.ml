(** This module specifies the structure of GCIC *)
open Common.Id

(** Terms in GCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of int
  (* Extras *)
  | Ascription of term * term

and fun_info =
  { id : Name.t
  ; dom : term
  ; body : term
  }

(** Returns the stringified version of a term *)
let rec to_string =
  let open Format in
  function
  | Var x -> Name.to_string x
  | Universe i -> asprintf "Universe_%i" i
  | App (t, t') -> asprintf "(%s %s)" (to_string t) (to_string t')
  | Lambda { id; dom; body } ->
    asprintf "fun %s : %s. %s" (Name.to_string id) (to_string dom) (to_string body)
  | Prod { id; dom; body } ->
    asprintf "Prod %s : %s. %s" (Name.to_string id) (to_string dom) (to_string body)
  | Unknown i -> asprintf "?_%i" i
  | Ascription (t, ty) -> asprintf "%s : %s" (to_string t) (to_string ty)
