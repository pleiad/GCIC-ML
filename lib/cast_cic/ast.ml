(** This module specifies the structure of CastCIC *)
open Common.Id

(** Global counter used to create new identifiers *)
let id_counter : int ref = ref 0

(** Returns a new identifier name *)
let new_identifier () : Name.t =
  let id = Name.of_string ("#" ^ string_of_int !id_counter) in
  id_counter := !id_counter + 1;
  id

(** Terms in CastCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of term
  | Err of term
  | Cast of
      { source : term
      ; target : term
      ; term : term
      }
  | Const of Name.t

and fun_info =
  { id : Name.t
  ; dom : term
  ; body : term
  }

(** Pretty printer *)
module Pretty = struct
  open Fmt

  (** Returns if a term requires a parenthesis for unambiguation *)
  let need_parens = function
    | Lambda _ | Prod _ | Cast _ -> true
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
    | Unknown ty -> pf ppf "?_%a" pp ty
    | Err ty -> pf ppf "err_%a" pp ty
    | Cast { source; target; term } ->
      pf ppf "@[<hov 1>⟨%a ⇐@ %a⟩@ %a@]" pp target pp source pp term
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

(** Head constructors *)
type head =
  | HProd
  | HUniverse of int

(** Returns the head constructor of a type *)
let head : term -> (head, string) result = function
  | Prod _ -> Ok HProd
  | Universe i -> Ok (HUniverse i)
  | _ -> Error "invalid term to get head constructor"

(** Returns the least precise type for the given head constructor, 
    at the provided level *)
let germ i : head -> term = function
  | HProd ->
    let cprod = Config.cast_universe_level i in
    let univ = Universe cprod in
    if cprod >= 0
    then Prod { id = Name.default; dom = Unknown univ; body = Unknown univ }
    else Err univ
  | HUniverse j -> if j < i then Universe j else Err (Universe i)

(** Checks if a term corresponds to a germ at the provided universe level *)
let is_germ i : term -> bool = function
  | Prod { id = _; dom = Unknown (Universe j); body = Unknown (Universe k) } ->
    Config.cast_universe_level i = j && j = k && j >= 0
  | Err (Universe j) -> i = j
  | Universe j -> j < i
  | _ -> false

(** Checks if a term corresponds to a germ for a level >= to the provided universe level.
    We are adding this predicate mostly for the Prod-Germ rule in reduction, 
    where we need to check that the type being cast to ? is not a germ for any 
    j >= i. We cannot build an arbitrary j there, so we resort to this.  
  *)
let is_germ_for_gte_level i : term -> bool = function
  | Prod { id = _; dom = Unknown (Universe j); body = Unknown (Universe k) } ->
    j >= Config.cast_universe_level i && j = k && j >= 0
  | Err (Universe j) -> j = i && Config.cast_universe_level i < 0
  | _ -> false

(** Checks if a term is in neutral form *)
let rec is_neutral : term -> bool = function
  | Var _ -> true
  | App (t, _)
  | Unknown t
  | Err t
  | Cast { source = Unknown (Universe _); target = _; term = t }
  | Cast { source = Universe _; target = t; term = _ }
  | Cast { source = Prod _; target = Prod _; term = t }
  | Cast { source = Prod _; target = t; term = _ }
  | Cast { source = t; target = _; term = _ } -> is_neutral t
  | _ -> false

(** Checks if a type t makes ?_t or err t canonical *)
let is_unknown_or_error_canonical term : bool =
  match term with
  | Universe _ | Unknown (Universe _) | Err (Universe _) -> true
  | _ -> is_neutral term

(** Checks if a term is in canonical form *)
let is_canonical : term -> bool = function
  | Universe _ | Lambda _ | Prod _ -> true
  | Unknown t -> is_unknown_or_error_canonical t
  | Err t -> is_unknown_or_error_canonical t
  | Cast { source = ty; target = Unknown (Universe i); term = _ } when is_germ i ty ->
    true
  | t -> is_neutral t

(* Module alias *)
module Context = Name.Map

(** Performs substitution inside a term *)
let rec subst ctx = function
  | Var x ->
    (try Context.find x ctx |> Option.fold ~none:(Var x) ~some:(fun v -> v) with
    | Not_found -> Var x)
  | Universe i -> Universe i
  | App (t, u) -> App (subst ctx t, subst ctx u)
  | Lambda fi ->
    Lambda
      { fi with
        dom = subst ctx fi.dom
      ; body = subst (Context.add fi.id None ctx) fi.body
      }
  | Prod fi ->
    Prod
      { fi with
        dom = subst ctx fi.dom
      ; body = subst (Context.add fi.id None ctx) fi.body
      }
  | Unknown t -> Unknown (subst ctx t)
  | Err t -> Err (subst ctx t)
  | Cast { source; target; term } ->
    Cast { source = subst ctx source; target = subst ctx target; term = subst ctx term }
  | Const x -> Const x

let subst1 x v = subst (Context.add x (Some v) Context.empty)

(** Checks if two terms are identifiable up to alpha-renaming *)
let rec alpha_equal t1 t2 =
  match t1, t2 with
  | Var x, Var y -> x = y
  | Universe i, Universe j -> i = j
  | App (t1, u1), App (t2, u2) -> alpha_equal t1 t2 && alpha_equal u1 u2
  | Lambda fi1, Lambda fi2 ->
    let x_id = new_identifier () in
    let x = Var x_id in
    let body1 = subst1 fi1.id x fi1.body in
    let body2 = subst1 fi2.id x fi2.body in
    alpha_equal fi1.dom fi2.dom && alpha_equal body1 body2
  | Prod fi1, Prod fi2 ->
    let x_id = new_identifier () in
    let x = Var x_id in
    let body1 = subst1 fi1.id x fi1.body in
    let body2 = subst1 fi2.id x fi2.body in
    alpha_equal fi1.dom fi2.dom && alpha_equal body1 body2
  | Unknown t1, Unknown t2 -> alpha_equal t1 t2
  | Err t1, Err t2 -> alpha_equal t1 t2
  | Cast ci1, Cast ci2 ->
    alpha_equal ci1.source ci2.source
    && alpha_equal ci1.target ci2.target
    && alpha_equal ci1.term ci2.term
  | Const x, Const y -> x = y
  | _ -> false

(** Checks if two terms are alpha consistent *)
let rec alpha_consistent t1 t2 : bool =
  match t1, t2 with
  | Var x, Var y -> x = y
  | Universe i, Universe j -> i = j
  | App (t1, u1), App (t2, u2) -> alpha_consistent t1 t2 && alpha_consistent u1 u2
  | Lambda fi1, Lambda fi2 ->
    let x_id = new_identifier () in
    let x = Var x_id in
    let body1 = subst1 fi1.id x fi1.body in
    let body2 = subst1 fi2.id x fi2.body in
    alpha_consistent fi1.dom fi2.dom && alpha_consistent body1 body2
  | Prod fi1, Prod fi2 ->
    let x_id = new_identifier () in
    let x = Var x_id in
    let body1 = subst1 fi1.id x fi1.body in
    let body2 = subst1 fi2.id x fi2.body in
    alpha_consistent fi1.dom fi2.dom && alpha_consistent body1 body2
  | _, Cast ci2 -> alpha_consistent t1 ci2.term
  | Cast ci1, _ -> alpha_consistent ci1.term t2
  | _, Unknown _ -> true
  | Unknown _, _ -> true
  | Const x, Const y -> x = y
  | _ -> false
