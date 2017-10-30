(**
 * 210lib/ArraySequenceG.fj.sml
 *
 * Implements SEQUENCE_G with
 *   type 'a seq = 'a ArraySlice.slice.
 *
 * ATTENTION: Requires mlton-spoonhower.
 *)
structure ArraySequenceG : SEQUENCE_G =
struct
  open Primitives

  open GranularityControl

  structure A = Array
  structure AS = ArraySlice
  structure MPA = MLton.Parallel.Array

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

  fun nth s i =
      AS.sub (s, i) handle Subscript => raise Range

  fun tabulate f n =
      if n < 0
      then raise Size
      else AS.full (MPA.tabulate (!g) f n)

  fun append (s, t) =
      let
        val (ns, nt) = (length s, length t)
        fun ith i = if i >= ns then nth t (i-ns) else nth s i
      in
        tabulate ith (ns+nt)
      end

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

  fun reduceSeq f b s =
      case splitMid s of
              EMPTY => b
            | ONE x => x
            | PAIR (l, r) =>
              f (reduceSeq f b l, reduceSeq f b r)

  fun reduce f b s =
      if length s <= !g
      then reduceSeq f b s
      else
          case splitMid s of
              EMPTY => b
            | ONE x => x
            | PAIR (l, r) =>
              f (Primitives.par (fn () => reduce f b l,
                                 fn () => reduce f b r))

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

  fun scanIncl f b s =
    let val (prefixes, final) = scan f b s
    in drop (append (prefixes, singleton final)) 1
    end

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

  fun bsearch cmp s x =
      let
          val size = length s
      in
          if size = 0
          then 0
          else if size = 1
          then
              let
                  val m = nth s 0
              in
                  case cmp (x, m)
                   of EQUAL => 0
                    | LESS => 0
                    | GREATER => 1
              end
          else
              let
                  val mididx = size div 2
                  val mid = nth s mididx
              in
                  case cmp (x, mid)
                   of EQUAL => mididx
                    | LESS => bsearch cmp (take s mididx) x
                    | GREATER => mididx + (bsearch cmp (drop s mididx) x)
              end
      end

  fun mergeSeq cmp (s, t) =
      let
          val ls = length s
          val is = ref 0

          val lt = length t
          val it = ref 0

          fun merger _ =
              if !is = ls
              then nth t (!it) before it := (!it) + 1
              else if !it = lt
              then nth s (!is) before is := (!is) + 1
              else
                  let
                      val vs = nth s (!is)
                      val vt = nth t (!it)
                  in
                      case cmp (vs, vt)
                       of LESS => (is := (!is) + 1;
                                   vs)
                        | EQUAL => (is := (!is) + 1;
                                    vs)
                        | GREATER => (it := (!it) + 1;
                                      vt)
                  end
      in
          AS.full (A.tabulate ((length s) + (length t), merger))
      end

  fun merge cmp (s, t) =
      if (length s) + (length t) <= !g
      then mergeSeq cmp (s, t)
      else
          case splitMid s
           of EMPTY => t
            | ONE m =>
              let
                  val idx = bsearch cmp t m
              in
                  append (take t idx, append (s, drop t idx))
              end
            | PAIR (sl, sr) =>
              let
                  val m = nth sr 0
                  val idx = bsearch cmp t m
                  val (tl, tr) = (take t idx, drop t idx)

                  val (mergel, merger) = par (fn () => merge cmp (sl, tl),
                                              fn () => merge cmp (sr, tr))
              in
                  append (mergel, merger)
              end

  fun sort cmp s = reduce (merge cmp) (empty ()) (map singleton s)

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
