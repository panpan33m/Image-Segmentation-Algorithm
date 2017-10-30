structure TestSuite =
struct
  (* * * Result * * *)
  (* A result specifies the output produced by the run of a function: a value,
   * a raised exception, or a timeout.
   *
   * We provide utility wrappers that run a function with or without a timer,
   * producing a Result.t. *)
  structure Result =
  struct

    datatype 'a t = Exn of exn
                  | Value of 'a
                  | Timeout

    type 'a result = 'a t

    fun compare cmp (x, y) =
      case (x, y) of
        (Exn x', Exn y') => exnName x' = exnName y'
      | (Value x', Value y') => cmp (x', y')
      | (Timeout, Timeout) => true
      | _ => false

    fun toStr aToStr r =
      case r of
        Exn e => "Exn " ^ exnName e
      | Value a => aToStr a
      | Timeout => "Timeout"

    fun timed (secs : IntInf.int) (f : 'a -> 'b) (input : 'a) : 'b result =
      Value (TimeLimit.timeLimit (Time.fromSeconds secs) f input)
        handle TimeLimit.TimeOut => Timeout
             | e => Exn e

    fun untimed (f : 'a -> 'b) (input : 'a) : 'b result =
      Value (f input)
        handle e => Exn e

  end


  (* * * Checkers * * *)
  (* A checker is essentially a function to test, f, along with a function,
   * ver, which checks whether the given output is correct with regards to
   * the given input.
   * We can generalize testing against fixed output, or a reference solution
   * to tetsing against a checker. *)
  structure Checker =
  struct

    type ('a, 'b) t = ('a -> 'b) * (('a * 'b Result.t) -> bool)
    type ('a, 'b) checker = ('a, 'b) t

    datatype ('a, 'b) correctness = Correct
                         | Incorrect of ('a * 'b)

    fun fromRefsol (f, fRef, cmp) : ('a, 'b) checker =
      let
        fun ver (input, result) =
          Result.compare cmp (result, Result.untimed fRef input)
      in
        (f, ver)
      end

    fun fromOutput (f, cmp) : (('a * 'b Result.t), 'b) checker =
      let
        fun f' (input, _) =
          f input
        fun ver ((input, desired), result) =
          Result.compare cmp (result, desired)
      in
        (f', ver)
      end

    fun fromVerifier (f, ver) : ('a,'b) checker = (f, ver)

    fun check resulter (f, ver) input =
      let
        val got = resulter f input
      in
        if ver (input, got) then Correct else Incorrect (input, got)
      end

    fun timedCheck secs checker input = check (Result.timed secs) checker input
    fun untimedCheck checker input = check Result.untimed checker input

  end


  (* * * Loggers * * *)
  (* Here we specify three logging levels which can be specified on a testcase
   * by testcase basis: Silent, Verbose, Desc str. *)
  structure Logger =
  struct

    datatype verbosity = Silent
                       | Verbose
                       | Desc of string

    type ('a, 'b) t = ('a -> string) * ('b -> string)
    type ('a, 'b) logger = ('a, 'b) t

    fun log (iToS, oToS) correctness verbosity =
      case correctness of
        Checker.Correct => print "Test passed.\n"
      | Checker.Incorrect (inp, out) =>
          case verbosity of
            Silent => print "Test failed.\n"
          | Verbose =>
              print ("Test failed on input " ^ iToS inp ^ "; got " ^
                     Result.toStr oToS out ^ ".\n")
          | Desc str => print ("Test failed: " ^ str ^ ".\n")

    fun create (iToS : 'a -> string, oToS : 'b -> string) : ('a, 'b) logger =
      (iToS, oToS)

    val dummy : ('a, 'b) logger =
      (fn _ => "DUMMY", fn _ => "DUMMY")

  end


  (* * * Tester * * *)
  (* Each testcase is a triple consisting of a point value, a test input, and
   * a verbosity specification as described above. *)
  structure Tester =
  struct

    type 'a testcase = (int * 'a * Logger.verbosity)

    fun test secs checker logger (pts, input, verbosity) =
      let
        val correctness = Checker.timedCheck secs checker input
        val _ = Logger.log logger correctness verbosity
      in
        (pts, case correctness of Checker.Correct => true | _ => false)
      end

    fun testGroup name secs checker logger testcaseList =
      let
        val _ = print "\n"
        val _ = print (name ^ ":\n")
        val results = List.map (test secs checker logger) testcaseList
        val _ = print "\n"
      in
        results
      end

  end

  (* * * Grader * * *)
  (* Some functions we commonly use to assign scores based on testcase
   * results. *)
  structure Grader =
  struct

    val rfi = Real.fromInt

    (* floor to the nearest multiple of m *)
    fun floorMultiple m x =
      Real.realFloor (x / m) * m;

    fun square x =
      Math.pow (x, 2.0)

    fun cube x =
      Math.pow (x, 3.0)

    (* calculate how many correct versus how many total, and pass to
     * generic scoring strategy to figure out the rest *)
    fun scoreFlexible strategy (tested : (int * bool) list) =
      let
        val total = List.foldl Int.+ 0 (List.map #1 tested)
        val correctList = List.filter #2 tested
        val correct = List.foldl Int.+ 0 (List.map #1 correctList)
      in
        Real.max (strategy correct total, 0.0)
      end

    (* cubic curve, floored to the nearest multiple of m *)
    fun cubicFloorMultiple m points correct total =
      floorMultiple m (points * (if total = 0 then 1.0
                                 else cube (rfi correct) / cube (rfi total)))

    fun squareFloorMultiple m points correct total =
      floorMultiple m (points * (if total = 0 then 1.0
                                 else square (rfi correct) / square (rfi total)))

    (* simple ratio correct/total *)
    fun simple points correct total =
      if total = 0 then 1.0
      else (rfi correct) / (rfi total) * points

    fun simpleFloorMultiple m points correct total =
      floorMultiple m (points * (if total = 0 then 1.0
                                 else (rfi correct) / (rfi total)))

    fun scoreCubicFloorMultiple m points tested =
      scoreFlexible (cubicFloorMultiple m points) tested

    fun scoreSquareFloorMultiple m points tested =
      scoreFlexible (squareFloorMultiple m points) tested

    fun scoreFloorMultiple m points tested =
      scoreFlexible (simpleFloorMultiple m points) tested

    fun score points tested =
      scoreFlexible (simple points) tested

    fun scoreCutoffsFloor points numCutoffs (tested : (int * bool) list) =
      raise Fail "deprecated"

    fun display nameScoreList =
      let
        val _ = print "\n"
        val _ = print "{ \"scores\": {"
        fun scoreToS (name, pts) =
          "\"" ^ name ^ "\": " ^ (Real.toString pts)
        val _ = print (String.concatWith ", " (List.map scoreToS nameScoreList))
        val _ = print "} }"
      in
        ()
      end

  end

  (* Various utilities for writing autograders! Very useful... *)
  structure Util =
  struct
    structure Option =
    struct
      fun toString tos xopt =
        case xopt of
          NONE => "NONE"
        | SOME x => "SOME " ^ tos x

      fun equal eq (xopt, yopt) =
        case (xopt, yopt) of
          (NONE, NONE) => true
        | (SOME x, SOME y) => eq (x, y)
        | _ => false
    end

    structure Pair =
    struct
      fun toString (tos1, tos2) (x, y) =
        String.concat ["(", tos1 x, ",", tos2 y, ")"]

      fun equal (eq1, eq2) ((a, b), (c, d)) =
        eq1 (a, c) andalso eq2 (b, d)
    end

    structure Triple =
    struct
      fun toString (tos1, tos2, tos3) (x, y, z) =
        String.concat ["(", tos1 x, ",", tos2 y, ",", tos3 z, ")"]

      fun equal (eq1, eq2, eq3) ((a, b, c), (d, e, f)) =
        eq1 (a, d) andalso eq2 (b, e) andalso eq3 (c, f)
    end

    structure List =
    struct
      fun toString tos l =
        "[" ^ String.concatWith "," (List.map tos l) ^ "]"

      fun zip (l1, l2) =
        case (l1, l2) of
          ([], _) => []
        | (_, []) => []
        | (x :: l1', y :: l2') => (x, y) :: zip (l1', l2')

      fun equal eq (l1, l2) =
        List.length l1 = List.length l2 andalso List.all eq (zip (l1, l2))

      fun sorted (cmp : 'a * 'a -> order) (l : 'a list) =
        let
          val isNone = not o isSome
          fun leq (x, y) = cmp (x, y) <> GREATER
          fun checkNext (curr, (prev, b)) =
            (SOME curr, b andalso (isNone prev orelse leq (valOf prev, curr)))
        in
          #2 (foldl checkNext (NONE, true) l)
        end
    end
  end

end
