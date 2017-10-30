structure IntElt : ELEMENT where type t = Int.int =
struct
  structure Hashing = Hashing
  type t = Int.int

  val default = 0
  val equal : (int*int) -> bool = op=
  val compare = Int.compare
  local
    open Hashing
    open Hashing.Word
  in
  val hashgen = salt o fromInt
  val hash = runHash o hashgen
  end
  val toString = Int.toString
end
