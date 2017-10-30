(* Everything below is identical to ORDTABLE, except for the following changes:
 *   addition of Val structure
 *   table is no longer polymorphic
 *   removal of collect, iteratePrefixes (these require polymorphic tables)
 *   addition of function reduceVal *)
signature AUG_ORDTABLE =
sig
  structure Key : ORDKEY
  structure Val : MONOID
  structure Seq : SEQUENCE

  type t
  type table = t

  structure Set : ORDSET
    sharing Set.Key = Key
    sharing type Set.Seq.t = Seq.t

  val size : table -> int
  val domain : table -> Set.t
  val range : table -> Val.t Seq.t
  val toString : table -> string
  val toSeq : table -> (Key.t * Val.t) Seq.t

  val find : table -> Key.t -> Val.t option
  val insert : table * (Key.t * Val.t) -> table
  val insertWith : (Val.t * Val.t -> Val.t) -> table * (Key.t * Val.t) -> table
  val delete : table * Key.t -> table

  val empty : unit -> table
  val singleton : Key.t * Val.t -> table
  val tabulate : (Key.t -> Val.t) -> Set.t -> table
  val fromSeq : (Key.t * Val.t) Seq.t -> table

  val map : (Val.t -> Val.t) -> table -> table
  val mapKey : (Key.t * Val.t -> Val.t) -> table -> table
  val filter : (Val.t -> bool) -> table -> table
  val filterKey : (Key.t * Val.t -> bool) -> table -> table

  val reduce : (Val.t * Val.t -> Val.t) -> Val.t -> table -> Val.t
  val iterate : ('a * Val.t -> 'a) -> 'a -> table -> 'a

  val union : (Val.t * Val.t -> Val.t) -> table * table -> table
  val intersection : (Val.t * Val.t -> Val.t) -> table * table -> table
  val difference : table * table -> table

  val restrict : table * Set.t -> table
  val subtract : table * Set.t -> table

  val $ : (Key.t * Val.t) -> table

  (* ordered table functions *)
  val first : table -> (Key.t * Val.t) option
  val last : table -> (Key.t * Val.t) option

  val prev : table * Key.t -> (Key.t * Val.t) option
  val next : table * Key.t -> (Key.t * Val.t) option

  val split : table * Key.t -> table * Val.t option * table
  val join : table * table -> table

  val getRange : table -> Key.t * Key.t -> table

  val rank : table * Key.t -> int
  val select : table * int -> (Key.t * Val.t) option
  val splitRank : table * int -> table * table

  (* augmentation *)
  val reduceVal : table -> Val.t
end
