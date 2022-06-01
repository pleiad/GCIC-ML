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
  (* Inductives *)
  | Inductive of Name.t * int * term list
  | Constructor of Name.t * term list
  | Match of
      { ind : Name.t
      ; discr : term
      ; z : Name.t
      ; pred : term
      ; f : Name.t
      ; branches : branch list
      }
  (* Extras *)
  | Ascription of term * term
  | UnknownT of int
  | Const of Name.t

and fun_info =
  { id : Name.t
  ; dom : term
  ; body : term
  }

and branch =
  { ctor : Name.t
  ; ids : Name.t list
  ; term : term
  }

(** Pretty printers *)

(** Pretty printer *)
module Pretty = struct
  open Fmt

  (** Returns if a term requires a parenthesis for unambiguation *)
  let need_parens = function
    | Lambda _ | Prod _ | Ascription _ -> true
    | _ -> false

  let rec group_lambda_args acc = function
    | Lambda { id; dom; body } -> group_lambda_args ((id, dom) :: acc) body
    | t -> List.rev acc, t

  let rec group_prod_args acc = function
    | Prod { id; dom; body } when not (Name.is_default id) ->
      group_prod_args ((id, dom) :: acc) body
    | t -> List.rev acc, t

  (** Pretty printer for term *)
  let rec pp ppf = function
    | Var x -> pf ppf "%a" Name.pp x
    | Universe i -> pf ppf "▢%i" i
    | App (t, t') -> pf ppf "@[%a@ %a@]" maybe_parens t maybe_parens t'
    | Lambda _ as t -> group_lambda_args [] t |> pp_lambda ppf
    | Prod { id; dom; body } as t ->
      if Name.is_default id
      then pf ppf "@[<hov 1>%a →@ %a@]" pp dom pp body
      else group_prod_args [] t |> pp_prod ppf
    | Unknown i -> pf ppf "?%i" i
    | Inductive (ind, level, params) ->
      pf ppf "@[%a{%a}@ %a@]" Name.pp ind int level (list pp) params
    | Constructor (ctor, args) -> pf ppf "@[%a@ %a@]" Name.pp ctor (list pp) args
    | Match { discr; z; pred; _ } ->
      pf ppf "@[match %a as %a return@ %a with@]" pp discr Name.pp z pp pred
    | Ascription (t, ty) -> pf ppf "@[%a ::@ %a@]" pp t pp ty
    | UnknownT i -> pf ppf "?▢%i" i
    | Const x -> pf ppf "%a" Name.pp x

  (** Pretty-prints an argument of a lambda or prod *)
  and pp_arg ppf (x, ty) = pf ppf "@[(%a : %a)@]" Name.pp x pp ty

  (** Pretty-prints a lambda *)
  and pp_lambda ppf (args, body) =
    pf ppf "@[<hov 1>λ%a.@ %a@]" (list ~sep:sp pp_arg) args pp body

  (** Pretty-prints a prod *)
  and pp_prod ppf (args, body) =
    pf ppf "@[<hov 1>Π%a.@ %a@]" (list ~sep:sp pp_arg) args pp body

  (** Adds parenthesis around a term if needed *)
  and maybe_parens ppf t = if need_parens t then parens pp ppf t else pp ppf t

  (** Returns the prettified version of a term *)
  let to_string = to_to_string pp

  (** Prints the prettified version of a term *)
  let print = pp Format.std_formatter
end

(** Pretty printer for term *)
let pp_term = Pretty.pp

(** Returns the prettified version of a term *)
let to_string = Pretty.to_string

(** Prints the prettified version of a term *)
let print = Pretty.print
