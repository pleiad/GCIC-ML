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
  | Lambda of Name.t * term * term
  | Prod of Name.t * term * term
  | Unknown of term
  | Err of term
  | Cast of { source: term; target: term; term: term }

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
type head = Prod | Universe of int
(* | Inductive *)

(** Returns the head constructor of a type *)
let head : term -> (head, string) result = function
  | Prod _ -> Ok Prod 
  | Universe i -> Ok (Universe i)
  | Var _ | App (_, _) | Lambda (_, _, _) | Unknown _ | Err _ | Cast _ ->
      Error "invalid term to get head constructor"

(** Returns the least precise type for the given head constructor, 
    at the provided level *)
let germ i h : term =
  match h with
  | Prod ->
      let cprod = cast_universe_level i in
      let univ : term = Universe cprod in
      if cprod >= 0 then Prod (Name.of_string "__", Unknown univ, Unknown univ) else Err univ
  | Universe j -> if j < i then (Universe j) else Err (Universe i)
