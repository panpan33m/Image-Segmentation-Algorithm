(* Everything below is identical to TABLE except for the addition of ordered
 * table functions, Key now ascribing to ORDKEY, and Set now ascribing to
 * ORDSET *)
signature ORDTABLE =
sig
  structure Key : ORDKEY
  structure Seq : SEQUENCE

  type 'a t
  type 'a table = 'a t

  structure Set : ORDSET
    sharing Set.Key = Key
    sharing type Set.Seq.t = Seq.t

  val size : 'a table -> int
  val domain : 'a table -> Set.t
  val range : 'a table -> 'a Seq.t
  val toString : ('a -> string) -> 'a table -> string
  val toSeq : 'a table -> (Key.t * 'a) Seq.t

  val find : 'a table -> Key.t -> 'a option
  val insert : 'a table * (Key.t * 'a) -> 'a table
  val insertWith : ('a * 'a -> 'a) -> 'a table * (Key.t * 'a) -> 'a table
  val delete : 'a table * Key.t -> 'a table

  val empty : unit -> 'a table
  val singleton : Key.t * 'a -> 'a table
  val tabulate : (Key.t -> 'a) -> Set.t -> 'a table
  val collect : (Key.t * 'a) Seq.t -> 'a Seq.t table
  val fromSeq : (Key.t * 'a) Seq.t -> 'a table

  val map : ('a -> 'b) -> 'a table -> 'b table
  val mapKey : (Key.t * 'a -> 'b) -> 'a table -> 'b table
  val filter : ('a -> bool) -> 'a table -> 'a table
  val filterKey : (Key.t * 'a -> bool) -> 'a table -> 'a table

  val reduce : ('a * 'a -> 'a) -> 'a -> 'a table -> 'a
  val iterate : ('b * 'a -> 'b) -> 'b -> 'a table -> 'b
  val iteratePrefixes : ('b * 'a -> 'b) -> 'b -> 'a table -> ('b table * 'b)

  val union : ('a * 'a -> 'a) -> ('a table * 'a table) -> 'a table
  val intersection : ('a * 'b -> 'c) -> 'a table * 'b table -> 'c table
  val difference : 'a table * 'b table -> 'a table

  val restrict : 'a table * Set.t -> 'a table
  val subtract : 'a table * Set.t -> 'a table

  val $ : (Key.t * 'a) -> 'a table

  (* ordered table functions *)
  val first : 'a table -> (Key.t * 'a) option
  val last : 'a table -> (Key.t * 'a) option

  val prev : 'a table * Key.t -> (Key.t * 'a) option
  val next : 'a table * Key.t -> (Key.t * 'a) option

  val split : 'a table * Key.t -> 'a table * 'a option * 'a table
  val join : 'a table * 'a table -> 'a table

  val getRange : 'a table -> Key.t * Key.t -> 'a table

  val rank : 'a table * Key.t -> int
  val select : 'a table * int -> (Key.t * 'a) option
  val splitRank : 'a table * int -> 'a table * 'a table
end
