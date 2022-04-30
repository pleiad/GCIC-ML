(** This module specifies the structure of CastCIC *)

(** An abstract type for identifiers*)
module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

(** A string instance of the ID abstract type *)
module String_id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.equal
end

module Name : ID = String_id

(** Terms in CastCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of term
  | Err of term
  | Cast of { source: term; target: term; term: term }

and fun_info = { id: Name.t; dom: term; body: term }

(** Returns the stringified version of a term *)
let rec to_string (t : term) =
  let open Format in
  match t with
  | Var x -> Name.to_string x
  | Universe i -> asprintf "Universe_%i" i
  | App (t, t') -> asprintf "(%s %s)" (to_string t) (to_string t')
  | Lambda {id; dom; body} ->
      asprintf "lambda %s : %s. %s" (Name.to_string id) (to_string dom)
        (to_string body)
  | Prod {id; dom; body} ->
      asprintf "Prod %s : %s. %s" (Name.to_string id) (to_string dom)
        (to_string body)
  | Unknown ty -> asprintf "?_%s" (to_string ty)
  | Err ty -> asprintf "err_%s" (to_string ty)
  | Cast {source; target; term} -> asprintf "<%s<-%s> %s" (to_string target) (to_string source) (to_string term)

(** Context *)
type context = (Name.t, term) Context.t

(** GCIC variants: Gradual, Normalizing and Shift *)
type gcic_variant = G | N | S

(** Parameter specifying the GCIC variant *)
let gcic_variant : gcic_variant = N

(** Computes the level of the universe of a dependent product, 
    given the levels of its domain and codomain  *)
let product_universe_level i j =
  match gcic_variant with G | N -> max i j | S -> max i j + 1

(** Computes the level of the universe for a cast between (? -> ?) and ? *)
let cast_universe_level i = match gcic_variant with G -> i | N | S -> i - 1

(** Head constructors *)
type head = HProd | HUniverse of int
(* | Inductive *)

(** Returns the head constructor of a type *)
let head : term -> (head, string) result = function
  | Prod _ -> Ok HProd 
  | Universe i -> Ok (HUniverse i)
  | Var _ | App (_, _) | Lambda _ | Unknown _ | Err _ | Cast _ ->
      Error "invalid term to get head constructor"

(** Returns the least precise type for the given head constructor, 
    at the provided level *)
let germ i h : term =
  match h with
  | HProd ->
      let cprod = cast_universe_level i in
      let univ : term = Universe cprod in
      if cprod >= 0 then Prod {id = Name.of_string "__"; dom = Unknown univ; body = Unknown univ} else Err univ
  | HUniverse j -> if j < i then (Universe j) else Err (Universe i)

(** Checks if a term icorresponds to a germ at the provided universe level *)
let is_germ i : term -> bool = function
  | Prod {id=_; dom=Unknown (Universe j); body=Unknown (Universe k)} when 
    cast_universe_level i == j && j == k && j >= 0 -> true
  | Err (Universe j) -> i == j
  | Universe j -> j < i
  | _ -> false


(** Checks if a term is in neutral form *)
let rec is_neutral : term -> bool = function
  | Var _ -> true
  | App (t, _) | Unknown t | Err t
  | Cast {source=Unknown (Universe _); target=_;      term=t}
  | Cast {source=Universe _;           target=t;      term=_}
  | Cast {source=Prod _;               target=Prod _; term=t}
  | Cast {source=Prod _;               target=t;      term=_}
  | Cast {source=t;                    target=_;      term=_} -> is_neutral t
  | _ -> false

  (** Checks if a type t makes ?_t or err t canonical *)
let is_unknown_or_error_canonical : term -> bool = function
  | Universe _ | Unknown (Universe _) | Err (Universe _) -> true
  | _ -> false

(** Checks if a term is in canonical form *)
let is_canonical : term -> bool = function
  | Universe _ | Lambda _ | Prod _ -> true
  | Unknown t -> is_unknown_or_error_canonical t
  | Err t -> is_unknown_or_error_canonical t
  | Cast {source=ty; target=Unknown (Universe i); term=_} -> is_germ i ty
  | t -> is_neutral t