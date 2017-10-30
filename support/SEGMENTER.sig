signature SEGMENTER =
sig
  structure Seq : SEQUENCE

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* (segment c (E, n)) evaluates to T
   * where
   *   c is the initial credit assigned to each vertex
   *   E is a sequence of edges (u, v, w)
   *   n is the number of vertices present in E (vertices are integers 0...n-1)
   *
   *   T is a collection of MST edges chosen for segmentation
   *)
  val segment : int -> (edge Seq.t * int) -> edge Seq.t
end
