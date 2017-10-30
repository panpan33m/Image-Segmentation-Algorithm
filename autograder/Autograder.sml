functor MkAutograder
  (val runTests : bool
   val showScore : bool) =
struct
  open TestSuite

  structure Seq = ArraySequence
  structure Rand = DotMix
  structure Table = MkTreapTable(structure Key = IntElt)

  structure StuSegmenter = MkBoruvkaSegmenter (structure Rand = Rand)

  structure RefSegmenter = MkSlowKruskalSegmenter (structure Seq = Seq)

  local open Seq in
    fun both (a, b) = a andalso b
    fun allTrue s = reduce both true s

    (* Undirects a sequence containly ONLY directed edges *)
    fun undirect E = flatten (map (fn (u,v,w) => %[(u,v,w),(v,u,w)]) E)

    (* Verifies that the edges in E connect all vertices in [0, n-1] *)
    fun isSpanning (E, n) =
      let
        val E' = map (fn (u,v,_) => (u,v)) (undirect E)
        val updates = collect Int.compare E'
        val G = inject (tabulate (fn _ => empty ()) n, updates)
        fun BFS X F =
          if length F = 0 then allTrue X
          else let
            val X' = inject (X, map (fn v => (v, true)) F)
            val F' = filter (not o (nth X')) (flatten (map (nth G) F))
          in BFS X' F'
          end
      in n = 0 orelse BFS (tabulate (fn _ => false) n) (singleton 0)
      end

    (* Verifies that the edges in E is a tree on the vertices in [0, n-1] *)
    fun isTree (E, n) : bool =
      (length E) = (n - 1)

    fun inGraph (S, E) : bool =
      let
        val E' = map (fn (u, v, w) => (u, v)) E
        val E'' = Table.collect E'
        val E''' = Table.map (Table.Set.fromSeq) E''
        fun findExisting (u, v, w) =
          case Table.find E''' u of
            SOME s => Table.Set.find s v
          | NONE => false
        val S' = filter findExisting S
      in
        length S = length S'
      end

    val bigCredit = valOf Int.maxInt

    fun verifier (_, Result.Exn _) = false
      | verifier (_, Result.Timeout) = false
      | verifier ((E, n), Result.Value stus) =
          let
            val ours = RefSegmenter.segment bigCredit (E, n)
            val minWt = reduce op+ 0 (map #3 ours)
            val stuWt = reduce op+ 0 (map #3 stus)
          in
            minWt = stuWt andalso
            isSpanning (stus, n) andalso
            isTree (stus, n) andalso
            inGraph (stus, E)
          end
  end

  fun testify inps =
    List.map (fn (w, E, n) => (w, (undirect (Seq.fromList E), n), Logger.Verbose)) inps

  fun testMST () =
    let
      fun student (E, n) = StuSegmenter.segment bigCredit (E, n)
      val edgetos = Util.Triple.toString (Int.toString, Int.toString, Int.toString)
      val itos = Util.Pair.toString (Seq.toString edgetos, Int.toString)
      val otos = Seq.toString edgetos
    in
      Tester.testGroup
        "MST"
        10
        (Checker.fromVerifier (student, verifier))
        (Logger.create (itos, otos))
        (testify Tests.tests)
    end

  fun run () =
    let
      val grade = Grader.scoreFloorMultiple 0.5
      val groups = if not runTests then [] else
        [
          ("MST", grade 65.0 (testMST ()))
        ]
    in
      if showScore
      then Grader.display (("compilation", 0.0) :: groups)
      else ()
    end
end

structure Autograder = MkAutograder (val runTests = true val showScore = false)
