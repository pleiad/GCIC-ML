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

let rec lookup key ctx : 'v =
  match ctx with
  | [] -> raise Not_found
  | (k, v) :: tl -> if k == key then v else lookup key tl

(** Returns the value associated to the given key.
      @raise Not_found if the given key has no associated value. *)
