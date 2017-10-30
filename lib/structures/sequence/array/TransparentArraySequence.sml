(**
 * 210lib/ArraySequence.sml
 *
 * Implements SEQUENCE_EXTRA with
 *   type 'a seq = 'a ArraySlice.slice
 *)

(* ========================================================================== *
 * ======= SEE BELOW FOR DEFINITION OF ArraySequence : SEQUENCE_EXTRA ======= *
 * ========================================================================== *)

structure ArraySequenceBase : SEQUENCE =
struct
  open Primitives

  structure A = Array
  structure AS = ArraySlice

  type 'a t = 'a AS.slice
  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

  val length : 'a seq -> int = AS.length
  fun empty _ = AS.full (A.fromList [])
  fun singleton x = AS.full (A.fromList [x])
  val $ = singleton

  fun tabulate f n =
    if n < 0 then raise Size else AS.full (A.tabulate (n, f))

  fun nth s i =
    AS.sub (s, i) handle Subscript => raise Range

  fun toString f s =
    "<" ^ String.concatWith "," (List.tabulate (length s, f o nth s)) ^ ">"

  fun fromList l = AS.full (A.fromList l)

  val % = fromList

  fun subseq s (i, len) =
    if len < 0
    then raise Size
    else AS.subslice (s, i, SOME len) handle Subscript => raise Range

  fun take s n = subseq s (0, n)
  fun drop s n = subseq s (n, length s - n)

  fun splitHead s =
    if length s = 0 then NIL else CONS (nth s 0, drop s 1)

  fun splitMid s =
    case length s of
      0 => EMPTY
    | 1 => ONE (nth s 0)
    | n => PAIR (take s (n div 2), drop s (n div 2))

  fun rev s =
    tabulate (fn i => nth s (length s - 1 - i)) (length s)

  fun append (s, t) =
    let val (ns, nt) = (length s, length t)
        fun ith i = if i >= ns then nth t (i-ns) else nth s i
    in tabulate ith (ns+nt)
    end

  fun iteratePrefixes f b s =
    let
      fun iter s (old, cur) =
        case splitHead s of
          NIL => (rev (fromList old), cur)
        | CONS (x, xs) => iter xs (cur::old, f (cur, x))
    in iter s ([], b)
    end

  fun iteratePrefixesIncl f b s =
    let val (prefixes, final) = iteratePrefixes f b s
    in drop (append (prefixes, singleton final)) 1
    end

  fun iterate f b s = #2 (iteratePrefixes f b s)
  fun toList s = iterate (fn (l,x) => x::l) [] (rev s)

  fun enum s = tabulate (fn i => (i, nth s i)) (length s)
  fun map f s = tabulate (f o (nth s)) (length s)
  fun mapIdx f s = tabulate (fn i => f (i, nth s i)) (length s)

  fun zipWith f (s, t) =
    tabulate (fn i => f (nth s i, nth t i)) (Int.min (length s, length t))

  fun unzipWith (spl : 'a -> 'b * 'c) s =
    let val s' = map spl s
    in (tabulate (#1 o nth s') (length s), tabulate (#2 o nth s') (length s))
    end

  fun zip (s, t) = zipWith (fn x => x) (s, t)
  fun unzip s = unzipWith (fn x => x) s

  fun reduce f b s =
    case splitMid s of
      EMPTY => b
    | ONE x => x
    | PAIR (l, r) =>
        f (Primitives.par (fn () => reduce f b l, fn () => reduce f b r))

  (* note: assuming b is an identity for f *)
  fun scan f b s =
    case length s of
      0 => (empty (), b)
    | 1 => (singleton b, nth s 0)
    | n =>
        let
          fun contract i =
            if i = n div 2 then nth s (2*i)
            else f (nth s (2*i), nth s (2*i+1))
          val (r, res) = scan f b (tabulate contract ((n+1) div 2))
          fun expand i =
            if i mod 2 = 0 then nth r (i div 2)
            else f (nth r (i div 2), nth s (i-1))
        in (tabulate expand n, res)
        end

  (* Alternative scan implementation doesn't assume b is an identity for f,
   * and combines b on the left after recursion. *)
  (*local
    fun scan' f S =
        if length S = 1 then (empty (), nth S 0)
        else let
          val n = length S
          fun contract i =
            if i = n div 2 then nth S (2*i)
            else f (nth S (2*i), nth S (2*i + 1))
          val S' = tabulate contract ((n+1) div 2)
          val (R, res) = scan' f S'
          fun expand 0 = nth S 0
            | expand i =
                if i mod 2 = 1 then nth R (i div 2)
                else f (nth R ((i-1) div 2), nth S i)
        in (tabulate expand (n-1), res)
        end
  in
    fun scan f b S =
      if length S = 0 then (empty (), b) else
      let val (R, res) = scan' f S
          val R' = map (fn x => f (b, x)) R
      in (append (singleton b, R'), f (b, res))
      end
  end*)

  (* alternative scan implementation: "up-sweep", "down-sweep" *)
  (*local
    (* reduce tree for nonempty sequences (explicitly annotate every node with
     * the reduced value and the length of that subsequence) *)
    datatype 'a tree = LEAF of 'a | NODE of 'a tree * 'a * int * 'a tree
  in
    fun scan f b s =
      if length s = 0 then (empty (), b) else
      let
        (* "up sweep" *)
        fun annotate s =
          case splitMid s of
            EMPTY => raise Fail "Not possible!"
          | ONE x => (LEAF x, x)
          | PAIR (l, r) =>
              let val ((l', lv), (r', rv)) =
                    Primitives.par (fn () => annotate l, fn () => annotate r)
                  val sv = f (lv, rv)
              in (NODE (l', sv, length s, r'), sv)
              end

        (* "down sweep"
         * Might be nicer to split this function into two parts:
         *   1. construct tree containing prefix-sums
         *   2. write tree into array *)
        fun writePrefixSums slice leftb t =
          case t of
            LEAF _ => AS.update (slice, 0, leftb)
          | NODE (l, _, _, r) =>
              let val (lv, llen) = case l of LEAF v => (v, 1)
                                           | NODE (_, v, llen, _) => (v, llen)
                  val lslice = AS.subslice (slice, 0, SOME llen)
                  val rslice = AS.subslice (slice, llen, NONE)
                  val rleftb = f (leftb, lv)
              in Primitives.par (fn () => writePrefixSums lslice leftb l,
                                 fn () => writePrefixSums rslice rleftb r); ()
              end

        val slice = tabulate (fn _ => b (* junk value *)) (length s)
        val (annotated, final) = annotate s
        val _ = writePrefixSums slice b annotated
      in
        (slice, final)
      end
  end*)

  fun scanIncl f b s =
    let val (prefixes, final) = scan f b s
    in drop (append (prefixes, singleton final)) 1
    end

  fun merge cmp (s, t) =
    let
      (* Sequential merge. Pretend it's parallel! *)
      fun merge' ([]   , t    ) = t
        | merge' (s    , []   ) = s
        | merge' (x::xs, y::ys) =
          if cmp (y, x) = LESS
          then y :: merge' (x::xs, ys)
          else x :: merge' (xs, y::ys)

    in fromList (merge' (toList s, toList t))
    end

  fun sort cmp s = reduce (merge cmp) (empty ()) (map singleton s)

  fun flatten ss =
    let
      val (starts, n) = scan op+ 0 (map length ss)
      val res = tabulate (fn _ => NONE) n
      fun write i (j,x) = AS.update (res, i+j, SOME x)
      val _ = map (fn (i,s) => mapIdx (write i) s) (zip (starts, ss))
    in map valOf res
    end

  fun filter p s =
    if length s = 0 then s else
    let
      val opts = map (fn e => if p e then SOME e else NONE) s
      val incrs = map (fn SOME _ => 1 | NONE => 0) opts
      val (psums, sz) = scan (op+) 0 incrs

      val res = AS.full (A.array (length s, nth s 0))
      fun update (SOME e, i) = AS.update (res, i, e)
        | update (NONE, _) = ()
    in
      AS.app update (zip (opts, psums));
      AS.subslice (res, 0, SOME sz)
    end

  fun filterIdx p =
    map (fn (_, x) => x) o (filter p) o enum

  fun equal cmp (s1,s2) =
    length s1 = length s2 andalso
    reduce (fn (x,y) => x andalso y) true (zipWith cmp (s1, s2))

  fun argmax cmp s =
    let
      fun best (a, b) =
        case (a, b) of
          (NONE, _) => b
        | (_, NONE) => a
        | (SOME (i, x), SOME (j, y)) =>
            if cmp (y, x) = GREATER then SOME (j, y) else SOME (i, x)
    in
      case reduce best NONE (mapIdx SOME s) of
        NONE => raise Range
      | SOME (i, _) => i
    end

  (* pretend it's parallel! *)
  fun inject (s, updates) =
    let
      val res = tabulate (nth s) (length s)
      fun update (i,x) = AS.update (res, i, x) handle Subscript => raise Range
    in
      AS.app update updates;
      res
    end

  fun update (s, (i, x)) = inject (s, singleton (i, x))

  fun collect cmp s =
    let
      val n = length s
      val (ks, vs) = unzip (sort (fn ((x,_), (y,_)) => cmp (x,y)) s)

      fun dk (0, _) = true
        | dk (i, k) = cmp (nth ks (i-1), k) <> EQUAL

      val starts = map (fn (i, _) => i) (filter dk (enum ks))
      val lengths = zipWith op- (drop (append (starts, %[n])) 1, starts)

      fun make (i, len) =
        if len < 0 then raise Size else (nth ks i, subseq vs (i, len))
    in
      zipWith make (starts, lengths)
    end

  fun collate cmp (s1, s2) =
    case (splitHead s1, splitHead s2) of
      (NIL, NIL) => EQUAL
    | (NIL, _  ) => LESS
    | (_  , NIL) => GREATER
    | (CONS (x, xs), CONS (y, ys)) =>
        case cmp (x, y) of
          EQUAL => collate cmp (xs, ys)
        | ord => ord
end

structure TransparentArraySequence :> SEQUENCE_EXTRA where type 'a t = 'a ArraySlice.slice =
struct
  open ArraySequenceBase

  (* Placeholder implementations. Not intended to be efficient. *)
  structure Fusion =
  struct
    fun mapMerge cmp (f, g) (s, t) =
      let val s' = map f s
          val t' = map g t
          fun cmp' ((x, _), (y, _)) = cmp (x, y)
      in map #2 (merge cmp' (zip (s, s'), zip (t, t')))
      end

    fun mapReduce f g b s = reduce g b (map f s)
    fun mapScan f g b s = scan g b (map f s)
    fun mapScanIncl f g b s = scanIncl g b (map f s)

    fun mapFilter f s = map Option.valOf (filter Option.isSome (map f s))
    fun mapFilterIdx f s = map Option.valOf (filter Option.isSome (mapIdx f s))

    (* Actual implementation would switch to f at some size larger than 1. This
     * implementation is nice because it is simple, and we don't care about
     * performance. *)
    fun shallowReduce f g s =
      case splitMid s of
        (EMPTY | ONE _) => f s
      | PAIR (l, r) => g (Primitives.par (fn () => shallowReduce f g l,
                                          fn () => shallowReduce f g r))
  end
end
