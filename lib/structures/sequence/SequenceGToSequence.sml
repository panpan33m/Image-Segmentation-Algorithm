functor SequenceGToSequence (structure Seq: SEQUENCE_G):>
        SEQUENCE_G_TO_SEQUENCE where type 'a listview = 'a Seq.listview =
struct
  structure S = Seq

  type 'a seq = 'a S.t
  type 'a ord = 'a S.ord
  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

  val defaultGranularity = 100
  val g = ref defaultGranularity

  fun setGranularity (newG: int): unit = g := newG

  fun mk1 (f : int -> 'a -> 'z): 'a -> 'z =
      fn a => f (!g) a

  fun mk2 (f : int -> 'a -> 'b -> 'z): 'a -> 'b -> 'z =
      fn a => fn b => f (!g) a b

  fun mk3 (f : int -> 'a -> 'b -> 'c -> 'z): 'a -> 'b -> 'c -> 'z =
      fn a => fn b => fn c => f (!g) a b c

  val nth : 'a seq -> int -> 'a = S.nth
  val length : 'a seq -> int = S.length
  val toList : 'a seq -> 'a list = fn x => (mk1 S.toList) x
  val toString : ('a -> string) -> 'a seq -> string = S.toString
  val equal : ('a * 'a -> bool) -> 'a seq * 'a seq -> bool =
      fn x => (mk2 S.equal) x

  val empty : unit -> 'a seq = S.empty
  val singleton : 'a -> 'a seq = S.singleton
  val tabulate : (int -> 'a) -> int -> 'a seq = fn x => (mk2 S.tabulate) x
  val fromList : 'a list -> 'a seq = S.fromList

  val rev : 'a seq -> 'a seq = fn x => (mk1 S.rev) x
  val append : 'a seq * 'a seq -> 'a seq = fn x => (mk1 S.append) x
  val flatten : 'a seq seq -> 'a seq = fn x => (mk1 S.flatten) x

  val filter : ('a -> bool) -> 'a seq -> 'a seq = fn x => (mk2 S.filter) x
  val map : ('a -> 'b) -> 'a seq -> 'b seq = fn x => (mk2 S.map) x
  val zip : 'a seq * 'b seq -> ('a * 'b) seq = fn x => (mk1 S.zip) x
  val zipWith : ('a * 'b -> 'c) -> 'a seq * 'b seq -> 'c seq =
      fn x => (mk2 S.zipWith) x

  val enum : 'a seq -> (int * 'a) seq = fn x => (mk1 S.enum) x
  val filterIdx : (int * 'a -> bool) -> 'a seq -> 'a seq =
   fn x => (mk2 S.filterIdx) x
  val mapIdx : (int * 'a -> 'b) -> 'a seq -> 'b seq =
   fn x => (mk2 S.mapIdx) x
  val update : 'a seq * (int * 'a) -> 'a seq = fn x => (mk1 S.update) x
  val inject : 'a seq * (int * 'a) seq -> 'a seq =
   fn x => (mk1 S.inject) x

  val subseq : 'a seq -> int * int -> 'a seq = fn x => (mk2 S.subseq) x
  val take : 'a seq -> int -> 'a seq = fn x => (mk2 S.take) x
  val drop : 'a seq -> int -> 'a seq = fn x => (mk2 S.drop) x
  val splitHead : 'a seq -> 'a listview = fn x => (mk1 S.splitHead) x
  val splitMid : 'a seq -> 'a treeview = fn x => (mk1 S.splitMid) x

  val iterate : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b =
   fn x => (mk3 S.iterate) x
  val iteratePrefixes : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq * 'b =
      fn x => (mk3 S.iteratePrefixes) x
  val iteratePrefixesIncl : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq =
      fn x => (mk3 S.iteratePrefixesIncl) x
  val reduce : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a =
   fn x => (mk3 S.reduce) x
  val scan : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq * 'a =
   fn x => (mk3 S.scan) x
  val scanIncl : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq =
   fn x => (mk3 S.scanIncl) x

  val sort : 'a ord -> 'a seq -> 'a seq = fn x => (mk2 S.sort) x
  val merge : 'a ord -> 'a seq * 'a seq -> 'a seq = fn x => (mk2 S.merge) x
  val collect : 'a ord -> ('a * 'b) seq -> ('a * 'b seq) seq =
      fn x => (mk2 S.collect) x
  val collate : 'a ord -> 'a seq ord = fn x => (mk1 S.collate) x
  val argmax : 'a ord -> 'a seq -> int = fn x => (mk2 S.argmax) x

  val $ : 'a -> 'a seq = S.$
  val % : 'a list -> 'a seq = S.%
end

structure ArraySequence:> SEQUENCE_G_TO_SEQUENCE =
SequenceGToSequence(structure Seq = ArraySequenceG)

structure TreeSequence:> SEQUENCE_G_TO_SEQUENCE =
SequenceGToSequence(structure Seq = TreeSequenceG)
