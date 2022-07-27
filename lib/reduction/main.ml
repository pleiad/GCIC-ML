module type Reduction = sig
  type t
  type o

  val reduce : t -> o
  val is_canonical : t -> bool
end
