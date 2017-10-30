signature EQKEY =
sig
  type t
  val equal : t * t -> bool
  val toString : t -> string
end
