module type Reduction = sig
  type t
  type o

  val reduce : t -> o
end