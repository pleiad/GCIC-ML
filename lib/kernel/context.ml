(** This module specifies the structure of Contexts. 
    Right now just implemented as a list *)

(* Does it make sense to just use the std Map module? *)

(** Instantiating to list *)
type ('k, 'v) t = ('k * 'v) list

(** The empty context *)
let empty : ('k, 'v) t = []

let is_empty = function
| [] -> true 
| _ -> false 

let add ~key ~value (ctx : ('k, 'v) t) : ('k, 'v) t = (key, value) :: ctx
(** Adds a new declaration to the context *)

let rec lookup ~key ~ctx : 'v =
  match ctx with
  | [] -> None
  | (k, v) :: tl -> if k == key then Some v else lookup ~key ~ctx:tl

(** Returns the value associated to the given key.
      @raise Not_found if the given key has no associated value. *)

let rec remove ~key ~ctx =
  match ctx with
  | [] -> []
  | (k, v) :: tl -> if k == key then tl else (k, v) :: remove ~key ~ctx
(** Remove a declaration from the context *)

let rec to_list = function
| [] -> []
| kv :: tl -> kv :: to_list tl
(** Converts a context into a list of key-value pairs *)

let rec of_list = function
| [] -> []
| kv :: tl -> kv :: of_list tl
(** Converts a list of key-value pairs into a context *)