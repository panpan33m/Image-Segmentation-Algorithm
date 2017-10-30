structure ListSequence :> SEQUENCE where type 'a t = 'a list =
struct
  open List

  type 'a t = 'a list
  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

  fun nth s i = List.nth (s, i) handle Subscript => raise Range
  fun toList (x : 'a seq) : 'a list = x
  fun toString f s = "<" ^ String.concatWith "," (map f s) ^ ">"

  fun empty () = []
  fun singleton x = [x]
  val $ = singleton
  fun tabulate f n = List.tabulate (n, f)
  fun fromList (x : 'a list) : 'a seq = x
  val % = fromList

  val append = op@
  val flatten = List.concat

  fun zipWith f ([], _) = []
    | zipWith f (_, []) = []
    | zipWith f (x::xs, y::ys) = f (x,y) :: zipWith f (xs, ys)

  fun zip (s1, s2) = zipWith (fn x => x) (s1, s2)

  fun equal eq (s1, s2) =
    length s1 = length s2 andalso List.all eq (zip (s1, s2))

  fun enum s =
    let
      fun addIdx (_, []) = []
        | addIdx (i, x::xs) = (i,x)::addIdx (i+1, xs)
    in addIdx (0, s)
    end

  fun filterIdx p = map (fn (_,x) => x) o (filter p) o enum
  fun mapIdx f = (map f) o enum

  fun subseq _ (0, 0) = []
    | subseq [] (0, _) = raise Size
    | subseq [] _ = raise Range
    | subseq (x::xs) (i, len) =
      case Int.compare (i, 0)
        of EQUAL => x::subseq xs (0, len-1)
         | LESS => raise Range
         | GREATER => subseq xs (i-1, len)

  fun take s n = List.take (s, n)
  fun drop s n = List.drop (s, n)

  fun splitHead [] = NIL
    | splitHead (x::xs) = CONS (x, xs)

  fun splitMid [] = EMPTY
    | splitMid [x] = ONE x
    | splitMid s = PAIR (take s (length s div 2), drop s (length s div 2))

  fun iterate f b s = List.foldl (fn (x,b) => f (b,x)) b s

  fun iteratePrefixes f b [] = ([], b)
    | iteratePrefixes f b (x::xs) =
      let
        val y = f (b, x)
        val (ys, r) = iteratePrefixes f y xs
      in (y::ys, r)
      end

  fun iteratePrefixesIncl f b s =
    let val (partials, final) = iteratePrefixes f b s
    in drop (append (partials, singleton final)) 1
    end

  fun reduce f b s =
    case splitMid s of
      EMPTY => b
    | ONE x => x
    | PAIR (l, r) =>
        f (Primitives.par (fn () => reduce f b l, fn () => reduce f b r))

  fun scan _ b [] = ([], b)
    | scan f b [x] = ([b], f (b, x))
    | scan f b s =
        let
          exception Mismatch
          fun contract [] = []
            | contract [x] = [x]
            | contract (x::y::z) = f (x, y)::contract z
          val (rs, result) = scan f b (contract s)
          fun expand ([], []) = []
            | expand ([r], [_]) = [r]
            | expand (r::rs, x::_::xs) = r::f (r, x)::expand (rs, xs)
            | expand _ = raise Mismatch
        in (expand (rs, s), result)
        end

  fun scanIncl f b s =
    let val (r, res) = scan f b s
    in drop (append (r, singleton res)) 1
    end

  fun merge _ ([], _) = []
    | merge _ (_, []) = []
    | merge cmp (x :: xs, y :: ys) =
        if cmp (y, x) = LESS
        then y :: merge cmp (x :: xs, ys)
        else x :: merge cmp (xs, y :: ys)

  fun sort cmp s = reduce (merge cmp) (empty ()) (map singleton s)

  fun inject (s, []) = s
    | inject (s, (i, x') :: updates) =
        if i < 0 then raise Range else
        let
          fun upd ([], _) = raise Range
            | upd (x :: xs, 0) = x' :: xs
            | upd (x :: xs, n) = x :: upd (xs, n-1)
        in inject (upd (s, i), updates)
        end

  fun update (s, (i, x)) = inject (s, singleton (i, x))

  fun unzipWith spl [] = ([], [])
    | unzipWith spl (p::ps) =
      let val (x, y) = spl p
          val (xs, ys) = unzipWith spl ps
      in (x::xs, y::ys)
      end

  fun unzip s = unzipWith (fn x => x) s

  fun collect cmp s =
      let
        fun gather k vs [] = ((k,vs), [])
          | gather k vs ((k',v)::rest) =
            if cmp (k,k') <> EQUAL then ((k,vs), (k',v)::rest)
            else gather k (v::vs) rest

        fun partition [] = []
          | partition ((k,v)::rest) =
            let val ((k, vs), rest') = gather k [v] rest
            in (k, rev vs)::partition rest'
            end
      in partition (sort (fn ((x,_), (y,_)) => cmp (x,y)) s)
      end

  fun collate _ ([], []) = EQUAL
    | collate _ ([], _) = LESS
    | collate _ (_, []) = GREATER
    | collate cmp (x::xs, y::ys) =
        case cmp (x, y) of
          EQUAL => collate cmp (xs, ys)
        | ord => ord

  fun argmax _ [] = raise Range
    | argmax cmp (x::xs) =
      let
        fun best ((i, x), (mi, mx)) =
          if cmp (x, mx) = GREATER then (i, x) else (mi, mx)
        val (idx, _) = List.foldl best (0, x) (enum (x::xs))
      in idx
      end
end
