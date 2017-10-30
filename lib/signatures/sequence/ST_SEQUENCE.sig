signature ST_SEQUENCE =
sig
  structure Seq : SEQUENCE

  type 'a t
  type 'a stseq = 'a t

  exception Range

  val fromSeq : 'a Seq.t -> 'a stseq
  val toSeq : 'a stseq -> 'a Seq.t
  val nth : 'a stseq -> int -> 'a

  val update : ('a stseq * (int * 'a)) -> 'a stseq
  val inject : ('a stseq * (int * 'a) Seq.t) -> 'a stseq
end
