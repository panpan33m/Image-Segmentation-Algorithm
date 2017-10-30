(**
 * 210lib/ArraySequence.fj.sml
 *
 * Implements SEQUENCE_EXTRA with
 *   type 'a seq = 'a ArraySlice.slice
 *
 * Implemented to be efficient and actually parallel.
 *)

(* ========================================================================= *
 * ===================== fast parallel implementations ===================== *
 * ========================================================================= *)
structure TransparentArraySequence :> SEQUENCE_EXTRA where type 'a t = 'a ArraySlice.slice =
struct

  (* see MLton's ARRAY_EXTRA -- gives us access to arrayUninit (allocating
   * but not initializing an array) *)
  structure A = ArrayExtra : ARRAY_EXTRA
  structure AS = ArraySliceExtra : ARRAY_SLICE_EXTRA
  structure G = SequenceGranularity

  type 'a t = 'a AS.slice
  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

  (*structure Serial = SerialArraySequence*)

  val length : 'a seq -> int = AS.length
  fun empty _ = AS.full (A.fromList [])
  fun singleton x = AS.full (A.array (1, x))
  val $ = singleton

  (* essentially a serial for loop with accumulator *)
  fun intIterate (f : 'a * int -> 'a) (b : 'a) (lo : int, hi : int) : 'a =
    let
      fun iter x i = if i = hi then x else iter (f (x, i)) (i+1)
    in
      iter b lo
    end

  fun serialFor (lo, hi) f =
    if hi - lo = 0 then () else (f lo; serialFor (lo+1, hi) f)

  fun parallelFor (lo, hi) (f : int -> 'a) : unit =
    case hi - lo of
      0 => ()
    | 1 => (f lo; ())
    | n => if n < G.get () then serialFor (lo, hi) f
           else let val mid = lo + (n div 2)
                in Primitives.par (fn () => parallelFor (lo, mid) f,
                                   fn () => parallelFor (mid, hi) f); ()
                end

  fun parallelForWithGran (g : int) (lo : int, hi : int) (f : int -> 'a) : unit =
    case hi - lo of
      0 => ()
    | 1 => (f lo; ())
    | n => if n < g then serialFor (lo, hi) f
           else let val mid = lo + (n div 2)
                in Primitives.par (fn () => parallelForWithGran g (lo, mid) f,
                                   fn () => parallelForWithGran g (mid, hi) f); ()
                end

  fun parallelFor (lo, hi) f = parallelForWithGran (G.get ()) (lo, hi) f
  fun parallelForNoGran (lo, hi) f = parallelForWithGran 1 (lo, hi) f

  fun tabulate f n =
    if n < 0 then raise Size else
    AS.full (MLton.Parallel.Array.tabulate (G.get()) f n)

  fun tabulate' n f = tabulate f n
  fun tabulateNoGran' n f = AS.full (MLton.Parallel.Array.tabulate 1 f n)

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
        fun ith i = if i < ns then nth s i else nth t (i-ns)
    in tabulate ith (ns + nt)
    end

  fun append3 (a, b, c) =
    let val (na, nb, nc) = (length a, length b, length c)
        fun ith i =
          if i < na      then nth a i else
          if i < na + nb then nth b (i - na)
          else                nth c (i - na - nb)
    in tabulate ith (na + nb + nc)
    end

  fun iterate f b s =
    let
      val n = length s
      fun iter (x, i) =
        if i = n then x else iter (f (x, nth s i), i+1)
    in
      iter (b, 0)
    end

  (* useful for things like toList, below *)

  fun writeIteratePrefixes result f b s =
    let
      val n = length s
      fun iter cur i =
        if i = n then cur
        else (AS.update (result, i, cur); iter (f (cur, nth s i)) (i+1))
    in iter b 0
    end

  fun iteratePrefixes f b s =
    let val result = AS.full (A.arrayUninit (length s))
    in (result, writeIteratePrefixes result f b s)
    end

  fun writeIteratePrefixesIncl result f b s =
    case length s of
      0 => ()
    | n =>
        let
          fun iter cur i =
            (AS.update (result, i, cur);
             if i = n-1 then () else iter (f (cur, nth s (i+1))) (i+1))
        in iter (f (b, nth s 0)) 0
        end

  fun iteratePrefixesIncl f b s =
    let val result = AS.full (A.arrayUninit (length s))
    in writeIteratePrefixesIncl result f b s;
       result
    end

  fun revIterate f b s =
    let
      fun iter x n =
        if n = 0 then x else iter (f (nth s (n-1), x)) (n-1)
    in
      iter b (length s)
    end
  fun toList s = revIterate op:: [] s

  fun enum s = tabulate (fn i => (i, nth s i)) (length s)
  fun map f s = tabulate (f o (nth s)) (length s)
  fun mapIdx f s = tabulate (fn i => f (i, nth s i)) (length s)

  fun zipWith f (s, t) =
    tabulate
      (fn i => f (nth s i, nth t i))
      (Int.min (length s, length t))

  fun unzipWith (f : 'a -> 'b * 'c) s =
    let
      val n = length s
      val (r1, r2) = (A.arrayUninit n, A.arrayUninit n)
    in
      parallelFor (0, n) (fn i =>
        let val (x1, x2) = f (nth s i)
        in A.update (r1, i, x1);
           A.update (r2, i, x2)
        end);
      (AS.full r1, AS.full r2)
    end

  fun zip (s, t) = zipWith (fn x => x) (s, t)
  fun unzip s = unzipWith (fn x => x) s

  (* TODO: double check this implementation...
   * QUESTIONS:
   *   is the base case too expensive? What if f is not constant time?
   *   similar problem: is the expansion step too expensive? or is it fine
   *     because we're only going to do it for at most k elements, a constant?
   *)
  fun scan (f : 'a * 'a -> 'a) (b : 'a) (s : 'a seq) : 'a seq * 'a =
    if length s <= G.get () then iteratePrefixes f b s else
    let
      val k = G.get () (* block size *)
      val n = length s
      val m = 1 + ((n - 1) div k) (* number of blocks *)

      val sums = tabulateNoGran' m
        (fn i => iterate f b (subseq s (i * k, Int.min (k, n - i * k))))
      val (partials, final) = scan f b sums

      val result = AS.full (A.arrayUninit n)
    in
      parallelForNoGran (0, m) (fn i =>
        writeIteratePrefixes
          (subseq result (i * k, Int.min (k, n - i * k)))
          f
          (nth partials i)
          (subseq s (i * k, Int.min (k, n - i * k))));
      (result, final)
    end

  structure Fusion =
  struct

    fun writeSerialMapMerge result cmp (f, g) (s, t) =
      let
        val (sn, tn) = (length s, length t)
        val n = sn + tn
      in
        intIterate (fn ((i,j), k) =>
          if j = tn then (AS.update (result, k, f (nth s i)); (i+1, j)) else
          if i = sn then (AS.update (result, k, g (nth t j)); (i, j+1)) else
          let val (x, y) = (nth s i, nth t j)
          in if cmp (x, y) <> GREATER
             then (AS.update (result, k, f x); (i+1, j))
             else (AS.update (result, k, g y); (i, j+1))
          end) (0,0) (0,n);
        ()
      end

    (* Returns number of elements of s which are strictly less than x *)
    fun lowRank cmp x s =
      let
        fun search (lo, hi) =
          case hi - lo of
            0 => lo
          | n =>
              let val half = n div 2
                  val mid = lo + half
              in case cmp (x, nth s mid) of
                   LESS => search (lo, mid)
                 | GREATER => search (mid + 1, hi)
                 | EQUAL =>
                     if (mid = 0) orelse (cmp (x, nth s (mid-1)) = GREATER)
                     then mid
                     else search (lo, mid)
              end
      in
        search (0, length s)
      end

    (* Returns number of elements of s which are <= x *)
    fun highRank cmp x s =
      let
        val m = length s
        fun search (lo, hi) =
          case hi - lo of
            0 => lo
          | n =>
              let val half = n div 2
                  val mid = lo + half
              in case cmp (x, nth s mid) of
                   LESS => search (lo, mid)
                 | GREATER => search (mid + 1, hi)
                 | EQUAL =>
                     if (mid = m-1) orelse (cmp (x, nth s (mid+1)) = LESS)
                     then mid+1
                     else search (mid+1, hi)
              end
      in
        search (0, m)
      end

    fun rankRange cmp x s = (lowRank cmp x s, highRank cmp x s)

    fun split s (i, j) =
      (subseq s (0, i), subseq s (i, j-i), subseq s (j, length s - j))

    fun writeMapMerge r cmp (f, g) (s, t) =
      case (length s, length t) of
        (n, 0) =>
          parallelFor (0, n) (fn i => AS.update (r, i, f (nth s i)))

      | (0, m) =>
          parallelFor (0, m) (fn i => AS.update (r, i, g (nth t i)))

      | (1, 1) =>
          let val (x, y) = (nth s 0, nth t 0)
              val (min, max) = if cmp (x, y) <> GREATER then (f x, g y) else (g y, f x)
          in AS.update (r, 0, min); AS.update (r, 1, max)
          end

      | (n, m) =>
          if n + m <= G.get () then writeSerialMapMerge r cmp (f, g) (s, t) else
          let val pivot = nth s (n div 2)
              val (smid1, smid2) = rankRange cmp pivot s
              val (tmid1, tmid2) = rankRange cmp pivot t
              val (sl, se, sg) = split s (smid1, smid2)
              val (tl, te, tg) = split t (tmid1, tmid2)
              val (rl, re, rg) = split r (smid1 + tmid1, smid2 + tmid2)
          in Primitives.par4 (fn () => writeMapMerge rl cmp (f, g) (sl, tl),
                              fn () => writeMapMerge rg cmp (f, g) (sg, tg),
                              fn () => parallelFor (0, length se) (fn i => AS.update (re, i, f (nth se i))),
                              fn () => parallelFor (0, length te) (fn i => AS.update (re, length se + i, g (nth te i))));
             ()
          end

    (* (mapMerge cmp (f, g) (s, t)) merges (map f s) with (map f t), except
     * that it compares elements according to their values before the map. *)
    fun mapMerge (cmp : 'a ord) (f : 'a -> 'b, g : 'a -> 'b) (s : 'a seq, t : 'a seq) : 'b seq =
      let val result = AS.full (A.arrayUninit (length s + length t))
      in writeMapMerge result cmp (f, g) (s, t); result
      end

    (* (mapReduce f g b s) is logically equivalent to (reduce g b (map f s)) *)
    fun mapReduce (f : 'a -> 'b) (g : 'b * 'b -> 'b) (b : 'b) (s : 'a seq) : 'b =
      let
        val k = G.get ()
        fun mapReduce' s =
          case splitMid s of
            EMPTY => b
          | ONE x => f x
          | PAIR (l, r) =>
              if length s <= k
              then g (mapReduce' l, mapReduce' r)
              else g (Primitives.par (fn () => mapReduce' l,
                                      fn () => mapReduce' r))
      in
        mapReduce' s
      end

    fun mapScan (f : 'a -> 'b) (g : 'b * 'b -> 'b) (b : 'b) (s : 'a seq) : 'b seq * 'b =
      if length s <= G.get ()
      then iteratePrefixes (fn (x, y) => g (x, f y)) b s
      else let
        val k = G.get ()
        val n = length s
        val m = 1 + ((n - 1) div k) (* number of blocks *)
        fun g' (x : 'b, y : 'a) : 'b = g (x, f y)

        val sums = tabulateNoGran' m
          (fn i => iterate g' b (subseq s (i * k, Int.min (k, n - i * k))))
        val (partials : 'b seq, final : 'b) = scan g b sums

        val result : 'b seq = AS.full (A.arrayUninit n)
      in
        parallelForNoGran (0, m) (fn i =>
          (writeIteratePrefixes
            (subseq result (i * k, Int.min (k, n - i * k)))
            g'
            (nth partials i)
            (subseq s (i * k, Int.min (k, n - i * k)))));
        (result, final)
      end

    (* TODO: direct implementation will be faster. *)
    fun mapScanIncl f g b s =
      let val (prefixes, final) = mapScan f g b s
      in drop (append (prefixes, singleton final)) 1
      end

    (* (mapFilterIdx f s) is logically equivalent to
     * (map valOf (filter isSome (mapIdx f s))) *)
    (*val mapFilterIdx : (int * 'a -> 'b option) -> 'a seq -> 'b seq*)
    fun mapFilterIdx (f : int * 'a -> 'b option) (s : 'a seq) : 'b seq =
      let
        val n = length s
        val ps = mapIdx f s (* get the predicate results up-front *)

        val bsize = G.get ()
        val nb = 1 + (n-1) div bsize (* number of blocks *)
        val bcounts : int seq = tabulateNoGran' nb (fn i =>
          let val lo = i * bsize
              val hi = Int.min ((i+1) * bsize, n)
              val block = subseq ps (lo, hi-lo)
          in iterate (fn (c, NONE) => c | (c, SOME _) => c+1) 0 block
          end)

        val (offsets, m) = scan op+ 0 bcounts
        val result = A.arrayUninit m
      in
        parallelForNoGran (0, nb) (fn i =>
          let
            val lo = i * bsize
            val hi = Int.min ((i+1) * bsize, n)
            fun update (offset, j) =
              case nth ps j of
                NONE => offset
              | SOME x' => (A.update (result, offset, x'); offset+1)
          in
            intIterate update (nth offsets i) (lo, hi)
          end);
        AS.full result
      end

    (* (mapFilter f s) is logically equivalent to
     * (map valOf (filter isSome (map f s))) *)
    (*val mapFilter : ('a -> 'b option) -> 'a seq -> 'b seq*)
    fun mapFilter f s = mapFilterIdx (f o #2) s

    (* (shallowReduce f g s) performs a reduce which switches to a
     * (usually sequential) implementation at small input sizes.
     * Formally it is equivalent to (reduce g b (map f s')) for some s' where
     * flatten s' = s, and where b = f (empty ()) *)
    (*val shallowReduce : ('a seq -> 'b) -> ('b * 'b -> 'b) -> 'a seq -> 'b*)
    fun shallowReduce f g s =
      let
        fun shallowReduce' s =
          let val n = length s
          in if n <= G.get () then f s
             else g (Primitives.par (fn () => shallowReduce' (take s (n div 2)),
                                     fn () => shallowReduce' (drop s (n div 2))))
          end
      in
        shallowReduce' s
      end

  end (* ArraySequence.Fusion *)

  fun reduce f b s = Fusion.mapReduce (fn x => x) f b s

  fun scanIncl f b s = Fusion.mapScanIncl (fn x => x) f b s
  (* TODO: direct implementation will be faster. *)
  (*fun scanIncl f b s =
    let val (prefixes, final) = scan f b s
    in drop (append (prefixes, singleton final)) 1
    end*)

  fun merge cmp (s, t) = Fusion.mapMerge cmp (fn x => x, fn x => x) (s, t)

  (* TODO: chunk s into blocks, sort each block sequentially,
   * then do mergesort *)
  fun sort cmp s = Fusion.mapReduce singleton (merge cmp) (empty ()) s

  (* TODO: can we make it faster? can we use techniques from filter, below,
   * i.e. explicit blocks? difficulty is that the elements of ss do not have
   * bounded size. one idea would be to allow blocks to cross the boundaries
   * of the nested sequences, although it is likely too costly to compute the
   * block boundaries. *)
  fun flatten ss =
    let
      val (offsets, n) = Fusion.mapScan length op+ 0 ss
      val result = A.arrayUninit n
    in
      parallelFor (0, length ss) (fn i =>
        let
          val offset = nth offsets i
          val elems = nth ss i
        in
          parallelFor (0, length elems) (fn j =>
            A.update (result, offset + j, nth elems j))
        end);
      AS.full result
    end

  (* splits s into blocks of size G.get(), with each block computed
   * sequentially. *)
  fun filterIdx p s =
    let
      val n = length s
      val ps = mapIdx p s (* get the predicate results up-front *)

      val bsize = G.get ()
      val nb = 1 + (n-1) div bsize (* number of blocks *)
      val bcounts = tabulateNoGran' nb (fn i =>
        let val lo = i * bsize
            val hi = Int.min ((i+1) * bsize, n)
            val block = subseq ps (lo, hi-lo)
        in iterate (fn (c, p) => if p then c+1 else c) 0 block
        end)

      val (offsets, m) = scan op+ 0 bcounts
      val result = A.arrayUninit m
    in
      parallelForNoGran (0, nb) (fn i =>
        let
          val lo = i * bsize
          val hi = Int.min ((i+1) * bsize, n)
          val block = subseq s (lo, hi-lo)
          fun update ((offset, j), x) =
            if nth ps j
            then (A.update (result, offset, x); (offset+1, j+1))
            else (offset, j+1)
        in
          iterate update (nth offsets i, lo) block; ()
        end);
      AS.full result
    end

  fun filter p s = filterIdx (p o #2) s

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

  (* inject is now nondeterministic, so we can blast all updates concurrently
   * and not worry about some being overwritten by others at conflicting
   * update locations.
   * TODO: what exactly are the semantics of AS.update, below? Is it
   * implemented as an atomic write? I.e., is this inject actually correct? *)
  fun inject (s, updates) =
    let
      val result = tabulate (nth s) (length s)
    in
      parallelFor (0, length updates) (fn i =>
        let val (idx, r) = nth updates i
        in AS.update (result, idx, r) handle Subscript => raise Range
        end);
      result
    end

  (* TODO: direct implementation may be slightly faster. Although, this function
   * is pretty much never used for ArraySequences, so perhaps we'll just ignore
   * it. *)
  fun update (s, (i, x)) = inject (s, singleton (i, x))

  (* TODO: can we make this faster? *)
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
