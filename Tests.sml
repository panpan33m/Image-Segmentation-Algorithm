structure Tests =
struct
  open List
  structure Seq = ArraySequence
                      
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight


  fun generate (n : int) (r : DotMix.rand) =
    let val (r', chooseSeed) = DotMix.splitTab (r, n)
        fun temper n = n mod 100 + 1
    in Seq.tabulate (temper o DotMix.int o chooseSeed) n
    end

  fun % L =
    let val asList = Seq.fromList L
        val weights = generate (Seq.length asList) (DotMix.fromInt 1337)
        val zipped = Seq.zip (asList,weights)
    in Seq.toList (Seq.map (fn ((a,b),c) => (a,b,c)) zipped) end
  fun %% (a,(b,c)) = (a,%b,c)

        
  fun wheel spokeNum =
    let
        val spokes = tabulate (spokeNum,fn i => (0, i + 1))
        val rubber = tabulate (spokeNum,fn i => (i+1, ((i+1) mod spokeNum)+1))
    in spokes@rubber end

  fun star spokeNum = tabulate (spokeNum,fn i => (0, i + 1))

  fun permuteE (edges, _) (x : int, y) =
    let fun flip i = if i = x then y else if i = y then x else i
    in map (fn (i, j) => (flip i, flip j)) edges end

  fun fullyConnected n =
    concat (tabulate (n, fn i => (tabulate (i,fn j => (i, j)))))

  val loner = ([(0,1)], 2)
  val line = ([(0,1),(1,2),(2,3)],4)
  val almostLine = ([(0,1),(1,2),(2,3),(1,4),(1,5)],6)
  val triangle = ([(0, 1), (1, 2), (2, 0)], 3)
  val square = ([(0,1),(1,2),(2,3),(3,0)],4)
  val tree = ([(0, 1), (1, 2), (1, 3), (0, 4), (4, 5), (4, 6),
               (4, 7), (7, 8), (8, 9), (9, 10), (9, 11)], 12)
  val bowtieEdge = ([(0, 1), (1, 2), (2, 0),
                      (2, 3), (3, 4), (4, 2)], 5)
  val bowtieCenter = (permuteE bowtieEdge (0, 2), 5)
  val hexagonCenter = (wheel 6, 7)
  val hexagonEdge = (permuteE hexagonCenter (0, 5), 7)
  val star10 = (star 10, 11)
  val full10 = (fullyConnected 10, 10)

  (* Each test case is a triple (w, edges, n), where `w` is a weight (for
   * scoring; set this value to 1 and ignore), `edges` is a list of
   * undirected weighted edges, and `n` is the number of vertices appearing in
   * `edges`. Correctness will be verified by checking that the output of the
   * following is a minimum spanning tree.
   *
   *   let
   *     fun undirect E =
   *       Seq.flatten (Seq.map (fn (u,v,w) => Seq.% [(u,v,w),(v,u,w)]) E)
   *   in
   *     segment (valOf Int.maxInt) (undirect (Seq.% edges), n)
   *   end
   *
   * It is very important that you
   *   (a) do not include "reverse" edges in the input (as you can see above,
   *   we will include these automatically for autograding), and
   *   (b) make sure the sum of all edge weights in an input graph is strictly
   *   less than (valOf Int.maxInt). *)
  val tests : (int * edge list * int) list = [
      (1, [(0,2,10), (1,0,5)], 3) ,
      %%(1,loner),
      %%(1,triangle),
      %%(1,tree),
      %%(1,bowtieEdge),
      %%(1,bowtieCenter),
      %%(1,hexagonCenter),
      %%(1,hexagonEdge),
      %%(1,star10),
      %%(1,full10),
      %%(1,line),
      %%(1,square)
  ]
  
end
