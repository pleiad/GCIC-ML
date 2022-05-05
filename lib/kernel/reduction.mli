(** This module specifies the operational semantics *)

open Cast_cic
open Reduction_term

(** The representation of a continuation of the CEK machine *)
type continuation =
| KHole
| KApp_l of (vterm * vcontext * continuation)
| KApp_r of (vfun_info * vcontext * continuation)
| KLambda of (Name.t * vterm * vcontext * continuation)
| KProd of (Name.t * vterm * vcontext * continuation)
| KUnknown of (vcontext * continuation)
| KErr of (vcontext * continuation)
| KCast_source of (vterm * vterm * vcontext * continuation)
| KCast_target of (vterm * vterm * vcontext * continuation)
| KCast_term of (vterm * vterm * vcontext * continuation)

(* Just an alias *)
type state = vterm * vcontext * continuation

(** One step reduction of terms *)
val reduce1 : state -> state

(** Transitive clousure of reduce1 with fuel *)
val reduce_fueled : int -> state -> vterm

(** Reduces a term in the given context *)
val reduce_in : context -> term -> term

(** Reduces a term *)
val reduce : term -> term