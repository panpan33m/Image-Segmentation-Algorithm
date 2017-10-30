functor SeqUtils (Seq : SEQUENCE) : SEQUENCE_UTILS = 
struct

    open Seq

    fun mapreduce f g b s = (reduce g b (map f s)) 

    fun filtermap f s = map valOf (filter isSome (map f s))

    fun filtermapIdx f s = (map valOf (filter isSome (mapIdx f s))) 

    (* FIXME: should be able to do this in O(n^2) work, O(1) span (for ArraySeq)
       with one big tabulate and some index math.  This adds a log(n) span for the 
       flatten and filter.  
     *)
    fun allDistinctPairsIdx s = 
        Seq.filter (fn ((i,_),(j,_)) => i <> j)
        (Seq.flatten (Seq.mapIdx (fn ix => Seq.mapIdx (fn jy => (ix,jy)) s) s))

    fun nonEmptySuffixes s = 
        Seq.tabulate (fn i => Seq.drop s i) (Seq.length s)

    fun max ord s = Seq.nth s (Seq.argmax ord s)

    fun cons x xs = Seq.append(Seq.singleton x, xs)

    fun toList2 s = Seq.toList (Seq.map Seq.toList s)

    datatype 'a listview' = NIL' | CONS' of 'a 
    fun splitHead' (s : 'a Seq.seq) = 
        case Seq.length s of 
            0 => NIL'
          | _ => CONS' (Seq.nth s 0)
        
    datatype 'a treeview'  = EMPTY' | ONE' of 'a | PAIR'
    fun splitMid' s =
        case Seq.length s of
            0 => EMPTY'
          | 1 => ONE' (Seq.nth s 0)
          | _ => PAIR'

    fun tail s n = Seq.drop s (Seq.length s - n)

    fun scan_nonempty (f : 'a * 'a -> 'a) (s : 'a Seq.seq) : 'a Seq.seq = 
      Seq.map (fn SOME x => x
                | NONE => raise Fail "called scan_nonempty on an empty sequence")
        (Seq.scanIncl (fn (SOME x,SOME y) => SOME (f(x,y))
                       | (NONE,z) => z
                       | (z,NONE) => z) 
                      NONE 
                      (Seq.map SOME s))

end