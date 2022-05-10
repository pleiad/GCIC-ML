(** This module specifies the structure of Contexts. 
    Right now just implemented as a list *)

(* Does it make sense to just use the std Map module? *)

type ('k, 'v) t = ('k * 'v) list
(** Instantiating to list *)

(** The empty context *)
let empty : ('k, 'v) t = []

let is_empty = function [] -> true | _ -> false

(** Adds a new declaration to the context *)
let add ~key ~value (ctx : ('k, 'v) t) : ('k, 'v) t = (key, value) :: ctx

let rec lookup ~key ~ctx : 'v =
  match ctx with
  | [] -> None
  (* Careful with this comparison? *)
  | (k, v) :: tl -> if k = key then Some v else lookup ~key ~ctx:tl

(** Returns the value associated to the given key. *)

(** Remove a declaration from the context *)
let rec remove ~key ~ctx =
  match ctx with
  | [] -> []
  (* Careful with this comparison? *)
  | (k, v) :: tl -> if k = key then tl else (k, v) :: remove ~key ~ctx:tl

(** Converts a context into a list of key-value pairs *)
let to_list ctx = ctx

(** Converts a list of key-value pairs into a context *)
let of_list ctx = ctx

let rec to_string string_of_key string_of_value = function
  | [] -> "[]"
  | (k, v) :: tl ->
      "(" ^ string_of_key k ^ ", " ^ string_of_value v ^ ") ; "
      ^ to_string string_of_key string_of_value tl
