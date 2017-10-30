(* Everything below is identical to SET except for the addition of the ordered
 * set functions and Key now ascribing to ORDKEY. *)
signature ORDSET =
sig
  structure Key : ORDKEY
  structure Seq : SEQUENCE

  type t
  type set = t

  val size : set -> int
  val toString : set -> string
  val toSeq : set -> Key.t Seq.t

  val empty : unit -> set
  val singleton : Key.t -> set
  val fromSeq : Key.t Seq.t -> set

  val find : set -> Key.t -> bool
  val insert : set * Key.t -> set
  val delete : set * Key.t -> set

  val filterKey : (Key.t -> bool) -> set -> set

  val reduceKey : (Key.t * Key.t -> Key.t) -> Key.t -> set -> Key.t
  val iterateKey : ('a * Key.t -> 'a) -> 'a -> set -> 'a

  val union : set * set -> set
  val intersection : set * set -> set
  val difference : set * set -> set

  val $ : Key.t -> set

  (* ordered sets *)
  val first : set -> Key.t option
  val last : set -> Key.t option

  val prev : set * Key.t -> Key.t option
  val next : set * Key.t -> Key.t option

  val split : set * Key.t -> set * bool * set
  val join : set * set -> set

  val getRange : set -> Key.t * Key.t -> set

  val rank : set * Key.t -> int
  val select : set * int -> Key.t option
  val splitRank : set * int -> set * set
end
