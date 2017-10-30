signature MONOID =
sig
  type t
  val I : t
  val f : t * t -> t

  val toString : t -> string
end
