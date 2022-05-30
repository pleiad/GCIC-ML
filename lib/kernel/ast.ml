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
  | UnknownT of int
  | Const of Name.t

and fun_info =
  { id : Name.t
  ; dom : term
  ; body : term
  }

(** Pretty printers *)

open Fmt

(** Pretty printer for term *)
let rec pp_term ppf =
  function
  | Var x -> pf ppf "%a" Name.pp x
  | Universe i -> pf ppf "▢%i" i
  | App (t, t') -> pf ppf "@[%a@ %a@]" pp_term t pp_term t'
  | Lambda { id; dom; body } ->
    pf ppf "@[λ(%a : %a).@ %a@]" Name.pp id pp_term dom pp_term body
  | Prod { id; dom; body } ->
    pf ppf "@[Π(%a : %a).@ %a@]" Name.pp id pp_term dom pp_term body
  | Unknown i -> pf ppf "?%i" i
  | Ascription (t, ty) -> pf ppf "%a : %a" pp_term t pp_term ty
  | UnknownT i -> pf ppf "?▢%i" i
  | Const x -> pf ppf "%a" Name.pp x

(** Returns the prettified version of a term *)
let to_string = to_to_string pp_term

(** Prints the prettified version of a term *)
let print = pp_term Format.std_formatter