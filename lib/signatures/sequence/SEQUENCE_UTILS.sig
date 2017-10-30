signature SEQUENCE_UTILS =
sig
  include SEQUENCE

  (* (mapReduce f g b s) is logically equivalent to (reduce g b (map f s)) *)
  val mapreduce : ('a -> 'b) -> ('b * 'b -> 'b) -> 'b -> 'a seq -> 'b

  (* (mapFilter f s) is logically equivalent to
     * (map valOf (filter isSome (map f s))) *)
  val filtermap : ('a -> 'b option) -> 'a seq -> 'b seq

  (* (mapFilterIdx f s) is logically equivalent to
     * (map valOf (filter isSome (mapIdx f s))) *)
  val filtermapIdx : (int * 'a -> 'b option) -> 'a seq -> 'b seq

  (* make the sequence < ... ((i,x_i),(j,x_j)) ... > for all distinct
     positions i and j in the input sequence *)
  val allDistinctPairsIdx : 'a seq -> ((int * 'a) * (int * 'a)) seq

  (* given a sequence <x1,...,xn>, 
     compute the sequence < <x1,...xn>, <x2,...,xn>, ..., <xn> > *)
  val nonEmptySuffixes : 'a seq -> ('a seq) seq 

  (* assumes sequence is non-empty; returns the max value according to the 'a ord *)
  val max : 'a ord -> 'a seq -> 'a 

  val cons : 'a -> 'a seq -> 'a seq

  val toList2 : ('a seq) seq -> ('a list) list
      
  (* if n <= length(s), returns the last n elements of s *)
  val tail : 'a seq -> int -> 'a seq

  (* like listview and treeview, but don't compute the tails/pairs,
     so its constant/log time instead of linear *)
  datatype 'a listview' = NIL' | CONS' of 'a 
  datatype 'a treeview'  = EMPTY' | ONE' of 'a | PAIR'

  val splitHead' : 'a seq -> 'a listview'
  val splitMid' : 'a seq -> 'a treeview'

  (* Assuming s is a non-empty sequence <x_0,...,x_n-1>, 
     scan_nonempty * s computes 
     <x_0 , x_0 * x_1, x_0 * x_1 * x_2, ..., x_0 * ... * x_n-1>.
     The result has the same length as s.  
   *)
  val scan_nonempty : ('a * 'a -> 'a) -> 'a seq -> 'a seq 

end
