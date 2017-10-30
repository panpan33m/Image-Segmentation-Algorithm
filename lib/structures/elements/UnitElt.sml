structure UnitElt : ELEMENT where type t = unit =
struct
  structure Hashing = Hashing
  type t = unit

  exception NYI

  val default = ()
  fun equal ((),()) = true
  fun compare ((),()) = raise NYI
  local
    open Hashing
    open Hashing.Word
  in
  fun hashgen () = salt (fromLarge 0wx0)
  val hash = runHash o hashgen
  end
  fun toString () = "()"
end
