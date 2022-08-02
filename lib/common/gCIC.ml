(** This module specifies the structure of GCIC *)
open Id

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
  | Fixpoint of fix_info

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

and fix_info =
  { fix_id : Name.t
  ; fix_body : term
  ; fix_type : term
  ; fix_rarg : int (* index of the recursive argument *)
  }

(** Pretty printers *)

(** Pretty printer *)
module Pretty = struct
  open Fmt

  (** Returns if a term requires a parenthesis for disambiguation *)
  let need_parens = function
    | Lambda _ | Prod _ | Ascription _ | Match _ | Fixpoint _ -> true
    | Inductive (_, _, args) | Constructor (_, args) -> args <> []
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
    | Universe 0 -> pf ppf "▢"
    | Universe i -> pf ppf "▢%i" i
    | App (t, t') -> pf ppf "@[%a@ %a@]" maybe_parens t maybe_parens t'
    | Lambda _ as t -> group_lambda_args [] t |> pp_lambda ppf
    | Prod { id; dom; body } as t ->
      if Name.is_default id
      then pf ppf "@[<hov 1>%a →@ %a@]" pp dom pp body
      else group_prod_args [] t |> pp_prod ppf
    | Unknown i -> pf ppf "?%i" i
    | Inductive (ind, _, []) -> pf ppf "%a" Name.pp ind
    | Inductive (ind, _, params) ->
      pf ppf "@[%a@ %a@]" Name.pp ind (list ~sep:sp maybe_parens) params
    | Constructor (ctor, []) -> pf ppf "%a" Name.pp ctor
    | Constructor (ctor, args) ->
      pf ppf "@[%a@ %a@]" Name.pp ctor (list ~sep:sp maybe_parens) args
    | Match { discr; z; pred; _ } ->
      pf
        ppf
        "@[match %a as %a return@ %a with@]"
        maybe_parens
        discr
        Name.pp
        z
        maybe_parens
        pred
    | Ascription (t, ty) -> pf ppf "@[%a ::@ %a@]" pp t pp ty
    | UnknownT i -> pf ppf "?▢%i" i
    | Const x -> pf ppf "%a" Name.pp x
    | Fixpoint { fix_body; _ } -> pf ppf "@[fix@ %a@]" maybe_parens fix_body

  (** Pretty-prints an argument of a lambda or prod *)
  and pp_arg ppf (x, ty) = pf ppf "@[(%a : %a)@]" Name.pp x pp ty

  (** Pretty-prints a lambda *)
  and pp_lambda ppf (args, body) =
    pf ppf "@[<hov 1>λ%a ⇒@ %a@]" (list ~sep:sp pp_arg) args pp body

  (** Pretty-prints a prod *)
  and pp_prod ppf (args, body) =
    pf ppf "@[<hov 1>∀%a,@ %a@]" (list ~sep:sp pp_arg) args pp body

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

let rec eq t1 t2 =
  match t1, t2 with
  | Var x, Var y -> x = y
  | Universe i, Universe j -> i = j
  | App (t1, u1), App (t2, u2) -> eq t1 t2 && eq u1 u2
  | Lambda fi1, Lambda fi2 ->
    fi1.id = fi2.id && eq fi1.dom fi2.dom && eq fi1.body fi2.body
  | Prod fi1, Prod fi2 -> fi1.id = fi2.id && eq fi1.dom fi2.dom && eq fi1.body fi2.body
  | Unknown i, Unknown j -> i = j
  | Inductive (n1, lvl1, ctors1), Inductive (n2, lvl2, ctors2) ->
    n1 = n2 && lvl1 = lvl2 && List.equal eq ctors1 ctors2
  | Constructor (n1, args1), Constructor (n2, args2) ->
    n1 = n2 && List.equal eq args1 args2
  | Match mi1, Match mi2 ->
    mi1.ind = mi2.ind
    && eq mi1.discr mi2.discr
    && mi1.z = mi2.z
    && eq mi1.pred mi2.pred
    && mi1.f = mi2.f
    && List.equal branch_eq mi1.branches mi2.branches
  | Ascription (t1, ty1), Ascription (t2, ty2) -> eq t1 t2 && eq ty1 ty2
  | UnknownT i, UnknownT j -> i = j
  | Const c1, Const c2 -> c1 = c2
  | Fixpoint fi1, Fixpoint fi2 -> eq_fix fi1 fi2
  | _ -> false

and eq_fix fi1 fi2 =
  fi1.fix_id = fi2.fix_id
  && eq fi1.fix_body fi2.fix_body
  && eq fi1.fix_type fi2.fix_type
  && fi1.fix_rarg = fi2.fix_rarg

and branch_eq b1 b2 =
  b1.ctor = b2.ctor && List.equal ( = ) b1.ids b2.ids && eq b1.term b2.term

let rec get_universe_lvl (t : term) =
  match t with
  | Universe lvl -> lvl
  | Prod fi -> get_universe_lvl fi.body
  | _ -> failwith "type of inductive definition must be a universe"

let rec prod_args : term -> Name.t list = function
  | Prod fi -> fi.id :: prod_args fi.body
  | _ -> []
