(* Here's a crappy, "correct" implementation for finding MSTs. Note that this
 * does not meet the cost bounds, and does not segment graphs properly (it
 * ignores credit) *)
functor MkSlowKruskalSegmenter
  (structure Seq : SEQUENCE)
  :> SEGMENTER where Seq = Seq =
struct
  structure Seq = Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  fun segment _ (E, n) =
    let
      fun allSame R =
        let
          val bottom = Seq.map (fn v => (v, true)) R
          val base = (Seq.nth R 0, true)
          fun combine ((v, a), (u, b)) =
            (v, v = u andalso a andalso b)
        in
          (#2 (Seq.reduce combine base bottom))
        end handle Range => true

      fun kruskalWouldNotBeProud (E, R, T) =
        if (allSame R) orelse (E = []) then (Seq.fromList T) else
        let
          val (u,v,w)::E' = E
          val repr = Seq.nth R
          fun redirect x =
            if (repr x) = (repr u) then (repr v) else (repr x)
          val R' = Seq.map redirect R
          val T' = if (repr u) = (repr v) then T else (u,v,w)::T
        in
          kruskalWouldNotBeProud (E', R', T')
        end

      fun edgeOrd ((_,_,w1),(_,_,w2)) = Int.compare (w1, w2)
      val presortedE = Seq.toList (Seq.sort edgeOrd E)
      val initR = Seq.tabulate (fn v => v) n
      val initT = []
    in
      kruskalWouldNotBeProud (presortedE, initR, initT)
    end
end
