(* ========================================================================= *
 * ====================== fast serial implementations ====================== *
 * ========================================================================= *)
structure TransparentSerialArraySequence :> SEQUENCE where type 'a t = 'a ArraySlice.slice =
struct

  (* see MLton's ARRAY_EXTRA -- gives us access to arrayUninit (allocating
   * but not initializing an array) *)
  structure A = ArrayExtra : ARRAY_EXTRA
  structure AS = ArraySliceExtra : ARRAY_SLICE_EXTRA

  type 'a t = 'a AS.slice
  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

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

  fun tabulate f n =
    if n < 0 then raise Size else
    AS.full (A.tabulate (n, f)) handle Size => raise Size

  fun tabulate' n f = tabulate f n

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
  fun revIterate f b s =
    let
      fun iter x n =
        if n = 0 then x else iter (f (nth s (n-1), x)) (n-1)
    in
      iter b (length s)
    end

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
      serialFor (0, n) (fn i =>
        let val (x1, x2) = f (nth s i)
        in A.update (r1, i, x1);
           A.update (r2, i, x2)
        end);
      (AS.full r1, AS.full r2)
    end

  fun zip (s, t) = zipWith (fn x => x) (s, t)
  fun unzip s = unzipWith (fn x => x) s

  fun reduce f b s =
    case splitMid s of
      EMPTY => b
    | ONE x => x
    | PAIR (l, r) => f (reduce f b l, reduce f b r)

  fun scan f b s = iteratePrefixes f b s
  fun scanIncl f b s = iteratePrefixesIncl f b s

  fun writeSerialMerge result cmp (s, t) =
    let
      val (sn, tn) = (length s, length t)
      val n = sn + tn
    in
      intIterate (fn ((i,j), k) =>
        if j = tn then (AS.update (result, k, nth s i); (i+1, j)) else
        if i = sn then (AS.update (result, k, nth t j); (i, j+1)) else
        let val (x, y) = (nth s i, nth t j)
        in if cmp (x, y) <> GREATER
           then (AS.update (result, k, x); (i+1, j))
           else (AS.update (result, k, y); (i, j+1))
        end) (0,0) (0,n);
      ()
    end

  fun merge cmp (s, t) =
    let val result = AS.full (A.arrayUninit (length s + length t))
    in writeSerialMerge result cmp (s, t); result
    end

  fun sort cmp s =
    case splitMid s of
      EMPTY => s
    | ONE x => singleton x
    | PAIR (l, r) => merge cmp (sort cmp l, sort cmp r)

  fun flatten ss =
    let
      val n = iterate (fn (c, s) => c + length s) 0 ss
      val result = A.arrayUninit n
    in
      intIterate (fn (offset, i) =>
        let val elems = nth ss i
        in serialFor (0, length elems) (fn j =>
             A.update (result, offset + j, nth elems j));
           offset + length elems
        end) 0 (0, length ss);
      AS.full result
    end

  fun filterIdx p s =
    let
      val ps = mapIdx p s
      val m = iterate (fn (c, p) => c + (if p then 1 else 0)) 0 ps
      val result = A.arrayUninit m
    in
      intIterate (fn (offset, i) =>
        if nth ps i
        then (A.update (result, offset, nth s i); offset+1)
        else offset) 0 (0, length s);
      AS.full result
    end

  fun filter p = filterIdx (p o #2)

  fun equal cmp (s1,s2) =
    length s1 = length s2 andalso
    intIterate (fn (b, i) => b andalso cmp (nth s1 i, nth s2 i)) true (0, length s1)

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

  fun inject (s, updates) =
    let
      val result = tabulate (nth s) (length s)
      fun update (i,x) = AS.update (result, i, x) handle Subscript => raise Range
    in
      AS.app update updates; result
    end

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
