(** This module specifies the operational semantics *)

open Cast_cic

(*
type continuation =
| KHole
| KApp_l of (term * context * continuation)
| KApp_r of (fun_info * context * continuation)
| KLambda of (Name.t * term * context * continuation)
| KProd of (Name.t * term * context * continuation)
| KUnknown of (context * continuation)
| KErr of (context * continuation)
| KCast_source of (term * term * context * continuation)
| KCast_target of (term * term * context * continuation)
| KCast_term of (term * term * context * continuation)

type state = term * context * continuation

val initial_state : term -> state

(** One step reduction of terms *)
val reduce1 : state -> state
*)

(** Transitive clousure of reduce1 *)
val reduce : term -> term