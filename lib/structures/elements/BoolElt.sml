structure BoolElt : ELEMENT where type t = Bool.bool =
struct
  structure Hashing = Hashing
  type t = Bool.bool

  exception NYI

  val default = true
  val equal : t * t -> bool = op=
  fun compare _ = raise NYI
  local
    open Hashing
    open Hashing.Word
  in
  fun hashgen true = salt (fromLarge 0wx1)
    | hashgen false = salt (fromLarge 0wx0)
  val hash = runHash o hashgen
  end
  val toString = Bool.toString
end
