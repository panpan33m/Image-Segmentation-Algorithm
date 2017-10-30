structure RealElt : ELEMENT where type t = Real64.real =
struct
  structure Hashing = Hashing
  type t = Real64.real

  val default = 0.0
  val equal = Real64.==
  val compare = Real64.compare
  val toString = Real64.toString
  local
    open Hashing
    open Hashing.Word
    fun realToWords (r: Real64.real) : word * word =
      let
        val vec = PackReal64Little.toBytes r
        val upperOrder = fromLarge (PackWord32Big.subVec (vec, 1))
        val lowerOrder = fromLarge (PackWord32Big.subVec (vec, 0))
      in
        (upperOrder, lowerOrder)
      end
  in
  fun hashgen r =
    let
      val (upper, lower) = realToWords r
    in
      combine (salt upper, salt lower)
    end
  val hash = runHash o hashgen
  end
end
