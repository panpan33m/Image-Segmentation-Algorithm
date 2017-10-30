signature PQ =
sig
  structure Key : ORDKEY

  type 'a t
  type 'a pq = 'a t

  val empty : unit -> 'a pq
  val singleton : (Key.t * 'a) -> 'a pq
  val fromList : (Key.t * 'a) list -> 'a pq

  val size : 'a pq -> int
  val findMin : 'a pq -> (Key.t * 'a) option

  val insert : 'a pq * (Key.t * 'a) -> 'a pq
  val deleteMin : 'a pq -> (Key.t * 'a) option * 'a pq
  val meld : 'a pq * 'a pq -> 'a pq

  val $ : (Key.t * 'a) -> 'a pq
  val % : (Key.t * 'a) list -> 'a pq
end
