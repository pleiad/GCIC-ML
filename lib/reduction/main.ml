module type Reduction = sig
  type t
  type o
  type error

  val reduce : t -> o
end