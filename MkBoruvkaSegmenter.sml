functor MkBoruvkaSegmenter
  (structure Rand : RANDOM210)
  :> SEGMENTER where Seq = ArraySequence =
struct
  structure Seq = ArraySequence
  exception NotYetImplemented
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  structure T : ORDTABLE = MkTreapTable(structure Key = IntElt)
  fun find! (t : 'a T.table) (x : int) : 'a = valOf (T.find t x)

  (* returns a table mapping each number in <0,...,n-1> to a random boolean,
     along with a new seed *)
  fun flipCoins (n : int) (r : Rand.rand) : (Rand.rand * bool T.table) =
      let
          val (r', chooseSeed) = Rand.splitTab (r, n)
      in (r', T.fromSeq (Seq.tabulate (fn i => (i , Rand.bool (chooseSeed i))) n))
      end

  (* Remove this exception when you're done! *)


  fun segment initialCredit (E, n) =
      let (*fun vertexJoiners E = *)
          val EL = Seq.map (fn (u,v,w) => (u,v,w,(u,v))) E
          val CreTableSeq = Seq.tabulate (fn x => T.singleton(x, initialCredit)) n
          val CreTable = Seq.reduce (fn(t1,t2) => T.union (fn (x,y) => x) (t1,t2)) (T.empty()) CreTableSeq
          val originalSeed = Rand.fromInt n;
          fun MST (EL, CreTable, T, n, seed) = (* EL : edge with label seq, T : edge seq, *)
            if (Seq.length EL = 0) then T
            else
              let
                val ET = Seq.map (fn (u,v,w,l) => T.singleton (u, (v,w,l))) EL
                fun joinEdges ((v1,w1,l1), (v2,w2,l2)) =
                    if (w1<=w2) then (v1,w1,l1)
                    else (v2,w2,l2)
                val VJT = Seq.reduce (fn (t1, t2) => (T.union joinEdges (t1,t2))) (T.empty()) ET (*returns a table mapping each vertex to its least weight neighbor*)

                (*fun joinerStarContract E r' = *)
                val (newR, RTable) = flipCoins n seed (* new seed!! *)
                val contractTable = T.filterKey (fn (u, (v,w,l)) => (*some qualified edges being contracted*)
                                    case ((find! RTable u), (find! RTable v)) of
                                         (false, true) => true
                                       | _ => false) VJT

                val P = T.mapKey (fn(u, (v,_,_)) => v) contractTable (* satellite->center *)


                val cTosT = Seq.map (fn(s,(c,w,l)) => (c,(s,w))) (T.toSeq contractTable) (* a seq of (center, (satellite * weight)) *)
                val centerT = T.collect cTosT (* center -> (satellite * weight) seq*)
                val updateCreTable = T.mapKey (fn(center, swSeq) =>
                                              let val (minC, totalWeight) =
                                                        Seq.reduce (fn((s1c,w1), (s2c,w2)) =>
                                                                      (Int.min (s1c,s2c), (w1+w2)))
                                                                    (find! CreTable center, 0) (Seq.map (fn (x,y) => (find! CreTable x, y)) swSeq)
                                                  in
                                                    Int.min(minC, find! CreTable center)  - totalWeight
                                                  end
                                            ) centerT

                val CreTable' = T.union (fn(c1,c2)=> c2) (CreTable, updateCreTable)

                val Ttable = T.mapKey (fn(u, (v,w,(l1,l2))) => (l1,l2,w)) contractTable (* edges already contracted *)
                val Tseq = Seq.map (fn(k,e) => e) (T.toSeq Ttable)
                val T' = Seq.append (T, Tseq)
                val Enofilter = Seq.map (fn(u,v,w,l) =>
                                  case ((T.find P u), (T.find P v)) of
                                      (SOME su, SOME sv) => (su,sv,w,l)
                                    | (SOME su, NONE) => (su,v,w,l)
                                    | (NONE, SOME sv) => (u,sv,w,l)
                                    | (NONE, NONE) => (u,v,w,l)
                                ) EL
                val E' = Seq.filter (fn(u,v,w,l) => u<>v) Enofilter (* stars left with their original edges *)
                val Echeckc = Seq.filter (fn(u,v,w,l) =>
                                              let val cu = find! CreTable' u
                                                  val cv = find! CreTable' v
                                              in (Int.min(cu,cv) >= w) end
                                          ) E'

              in
                  MST (Echeckc, CreTable', T', n, newR)
              end
      in
        MST (EL, CreTable, Seq.empty(), n, originalSeed)
      end


end
