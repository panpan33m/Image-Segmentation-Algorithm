(**
 * 210lib/ArraySequence.sml
 *
 * Implements SEQUENCE with
 *   type 'a seq = 'a ArraySlice.slice
 *
 * Author: Umut A. Acar
 * Copyright: Umut A. Acar @ 2016
 *
 * 
 *)
structure ParallelArraySequence : SEQUENCE =
struct
  open Primitives

  val THRESHOLD = 1
  val DEBUG = false

  structure A = Array

  (* A sequence is an array with begin and end indices, inclusive *)  
  datatype 'a elt = NULL | ELT of 'a 
   
  type 'a seq = int * int * ('a elt) A.array
  type 'a t = 'a seq
  type 'a ord = 'a * 'a -> order

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size
  exception Bug


  (** Debugging  **)
  fun dPrint str = 
    if DEBUG then 
      print str
    else 
      ()

  fun arrayToString f s a = 
    Array.foldri (fn (i, x, str) => f (i, x, str)) s a 

  (** Begin: Utilities and constructors
   **
   ** These include effectful operations. The rest of the code is pure.  
   **
   **)
  
  fun empty () = (0,~1,A.array (0,NULL))

  fun wrap x = ELT x

  fun unwrap x = 
    case x of 
      NULL => raise Bug
    | ELT y => y

  fun serialFor f b e = 
    if b = e then 
      f b
    else 
      (f b; serialFor f (b+1) e)

  fun parallelFor f b e : unit = 
    if e - b + 1 <= THRESHOLD then 
      let 
        val _ = dPrint ("pfor: serial b = " ^ Int.toString b ^ 
                       " e = " ^ Int.toString e ^ "\n") 
      in 
        serialFor (fn j => f j) b e 
      end
    else 
      let 
        val mid = Int.div (b + e, 2)
        val _ = dPrint ("pfor: parallel b = " ^ Int.toString b ^ 
                       " e = " ^ Int.toString e ^ 
                       " mid = " ^ Int.toString mid ^ 
                       "\n") 

        (* Very important that this is mid and mid+1, because mid can
        be biased towards the left half. *)

        val _ = Primitives.par (fn () => parallelFor f b mid, fn () => parallelFor f (mid+1) e)
      in
        ()
      end 

  (* TODO: replace with mlton allocate without initialization *)
  fun allocArray n = A.tabulate (n, fn i => NULL)

  fun tabulateArray n f = A.tabulate (n, f)

  fun alloc n = 
    let
      val _ = dPrint ("alloc: n = " ^ Int.toString n ^ "\n") 
      val a = A.tabulate (n, fn i => NULL)
    in 
      (0, n-1, a)
    end

  (* Note the indices  runs from 0 on *)
  fun initialize f (s as (b, e, a))  = 
    let 
      val init = parallelFor (fn j => A.update (a, b+j, wrap (f j))) 
     in
       init 0 (e-b)
     end

  (* No index trickiness. *)
  fun apply (s as (b, e, a): 'a seq) (f: 'a -> unit) = 
    let 
      val app =  parallelFor (fn j => f (unwrap (A.sub (a, j))))
    in
      app b e 
    end

  fun update (s as (b, e, a), (i, x)) = 
    if b <= i andalso i <= e then
      let 
        val () = A.update (a, i, wrap x)
      in
        (b, e, a)
      end 
    else
      raise Range
      
  (* This does not fit the semantics.  Writes our out of order. *)
  fun inject (s as (b, e, a), updates) =
    let
      val n = (e-b+1)

      (* copy to result *)
      val resultArray = allocArray n
      val _ = parallelFor (fn i => A.update (resultArray, i, A.sub (a,b+i))) 0 (n-1)

      fun writeToResult (i,x) = A.update (resultArray, i, wrap x) 
                                handle Subscript => raise Range

      val () = apply updates writeToResult
    in
      (0,e-b,resultArray)
    end

  (* It seems that we need this as a primitive. *)
  (* array a has some size n
     m is the number of SOME's in it
     positions is the position of each SOME in the compacted array
   * Example: 
     a = ELT NONE | ELT SOME 0 | ELT SOME 1 | ELT NONE | ELT SOME 2
     positions = ELT 0 | ELT 0 | ELT 1 | ELT 1| ELT 2         
     m = 3

     Algorithm: 
     First map this to 
     result = ELT SOME 0 | ELT SOME 1 | ELT SOME 2
     result' = 0 | 1 | 2
  *)  
  fun compactArray (a: 'a option elt A.array) 
                   (positions: int elt A.array) 
                   (m: int) 
                   : 'a elt A.array =
    let
      val _ = dPrint ("compactArray: start m = " ^ Int.toString m ^ "\n")

      val result = allocArray m
      val _ = dPrint ("compactArray: alloc done \n")

      fun f (i,x) = 
      let 
      in      
        case x of
          NULL => raise Bug
        | ELT y => 
            case y of 
              NONE => ()
            | SOME z =>
                let 
                  val pos = unwrap (Array.sub (positions, i))
                in 
                  A.update (result, pos, ELT z)       
                  handle e => (dPrint ("compactArray: update failed " ^ 
                                  "i = " ^  (Int.toString i) ^ 
                                  "pos = " ^ (Int.toString pos) ^
                                  "\n");                           
                           raise e)
                 end
      end
      (* Make a compact array, by sending each element to its destination.*)
      (* Must be an option type because of pre-allocation of result. *)
      val _ = A.appi f a
      val _ = dPrint ("compactArray: appi ended \n")

      (* Eliminate option types *)
(*
      val result' = tabulateArray m (fn i => unwrap (A.sub (result,i)))
*)
      val _ = dPrint ("compactArray: ended \n")
     in 
       result
     end
  (** End: Utilities **)

  (** Mainline **)

  fun tabulate f n = 
    let 
      val _ = dPrint "tabulate\n"
      val r = 
    if n < 0 then 
      raise Size
    else if n = 0 then 
      empty ()
    else 
      let 
      val _ = dPrint "tabulate: alloc\n"
      val s = alloc n handle x => (print "Exception in alloc"; raise x)
      val _ = dPrint "tabulate: alloc done\n"
        val () = initialize f s
      val _ = dPrint "tabulate: initialize done\n"
      in
        s
      end
      val _ = dPrint "tabulate done\n"
    in 
      r 
    end

  fun nth (s as (b,e,a)) i =       
    unwrap (A.sub (a, b+i)) handle Subscript => raise Range
 
  fun length (s as (b,e,A)) = 
    e-b+1


  fun singleton x = 
    (0, 0, A.array (1, wrap x))

  val $ = singleton

  fun append (s as (sb,se,sa), t as (tb,te,ta)) =
    let
      val ns = length s
      val nt = length t
     
      fun ith i = if i >= ns then nth t (i-ns) else nth s i

    in 
      tabulate ith (ns+nt)
    end

  fun toString f s =
    "<" ^ String.concatWith "," (List.tabulate (length s, f o nth s)) ^ ">"

  (* This is sequential. *)
  fun fromList l =
    let

      val () = dPrint ("fromList: Length = " ^ Int.toString (List.length l) ^ "\n")
      val result = 
      if List.length l = 0 then 
        empty ()
      else 
        (0, List.length l - 1, A.fromList (List.map wrap l))
      val () = dPrint ("fromList: result length = " ^ Int.toString (length result) ^ "\n")  in
       result
     end

  val % = fromList

  fun subseq (s as (b,e,a)) (i, len) =
    let
      val _ = dPrint ("subseq i = " ^ (Int.toString i) ^
                     " len = " ^ (Int.toString len) ^
                     " b = " ^ (Int.toString b) ^
                     " e = " ^ (Int.toString e) ^
                     "\n")
    in
      if len < 0 then 
        (print "exception Size\n" ; raise Size)
      else if i < 0 orelse i > (e-b+1) then 
        (print "exception Range\n" ; raise Range)
      else 
        let 
          val right = (b+i+len-1) handle _ => raise Range 
        in
          if right > e then 
            (print "exception Range\n" ; raise Range)
         else 
          (b+i,b+i+len-1,a)
        end
     end

  (* take the first m elements of the sequence. *)
  fun take s m = 
    if m < 0 then
      raise Size
    else 
      let 
        val r = subseq s (0, m)
      in
        r
      end

  (* drop the first i elements of the sequence *)
  fun drop s i = 
    let 
      val _ = dPrint ("drop i = " ^ (Int.toString i) ^
                      "length s = " ^ (Int.toString (length s)) ^
                      "\n")

      val r = subseq s (i, length s - i)  

      val _ = dPrint "drop done\n"
    in
      r
    end

(*
  l = 5
  i = 2
  1 2 3 4 5
      3 4 5
*)
  fun splitHead s =
    if length s = 0 then NIL else CONS (nth s 0, drop s 1)

  fun splitMid s =
    case length s of
      0 => (dPrint "splitMid, 0\n"; EMPTY)
    | 1 => (dPrint "splitMid, 1\n"; ONE (nth s 0))
    | n => (dPrint ("splitMid, n = " ^ Int.toString n ^ "\n"); PAIR (take s (n div 2), drop s (n div 2)))

  fun rev (s as (b,e,a)) =
    tabulate (fn i => nth s (length s - 1 - i)) (length s)

  fun iteratePrefixes f base s =
    let
      val (b,e,a) = s

      fun loop cur out i = 
        if i > e then 
          (fromList (List.rev out), cur)
        else 
          let 
            val x = unwrap (A.sub (a,i))
          in
            loop (f (cur,x)) (cur::out) (i+1)
          end      

    in loop base [] b
    end

  fun iteratePrefixesIncl f base s =
    let val (prefixes, final) = iteratePrefixes f base s
    in drop (append (prefixes, singleton final)) 1
    end

  fun iterate f b s = #2 (iteratePrefixes f b s)

  fun toList s = iterate (fn (l,x) => x::l) [] (rev s)

  fun enum s = tabulate (fn i => (i, nth s i)) (length s)

  fun map (f: 'a -> 'b) (s: 'a seq) : 'b seq = tabulate (f o (nth s)) (length s)

  fun mapIdx f s = tabulate (fn i => f (i, nth s i)) (length s)

  fun zipWith f (s, t) =
    let 
      val _ = dPrint "zipWith:\n"
      val r = tabulate (fn i => f (nth s i, nth t i)) (Int.min (length s, length t))
      val _ = dPrint "zipWith Done:\n"
    in
      r
    end

  fun unzipWith (spl : 'a -> 'b * 'c) s =
    let val s' = map spl s
    in (tabulate (#1 o nth s') (length s), tabulate (#2 o nth s') (length s))
    end

  (* TODO: direct implementations would probably be more efficient *)
  fun zip (s, t) = zipWith (fn x => x) (s, t)
  fun unzip s = unzipWith (fn x => x) s


  fun reduce f base s =
    let 
      val _ = dPrint "reduce\n"
    in 
      case splitMid s of
        EMPTY => base
      | ONE x => x
      | PAIR (l, r) =>
        let 
          val p = Primitives.par (fn () => reduce f base l, fn () => reduce f base r)
        in 
          f p 
        end
     end

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

  fun equal cmp (s1,s2) =
    let
       val _ = dPrint ("equal: \n")
       fun f (x,y) = 
         let 
           val _ = dPrint ("equal: x = " ^ Bool.toString x ^ " " ^ Bool.toString y ^ "\n")
         in
           x andalso y
         end

       fun cmp' (x,y) = 
         let 
           val r = cmp (x,y)
           val _ = dPrint ("cmp  = " ^ Bool.toString r ^ "\n")
         in
           r
         end


       val l1 = length s1 
       val l2 = length s2 
       val _ = print ("equal: lengths: " ^ (Int.toString l1 ) ^ " and " ^
                     (Int.toString l2 ) ^ "\n")
       val lMatch = length s1 = length s2 
       val _ = print ("equal: lengths match: " ^ (Bool.toString lMatch) ^ "\n")
       val s1s2 = zipWith cmp' (s1, s2)
       val _ = print "equal: reduce now\n "
       val r = reduce f true s1s2 
       val _ = print "equal: reduce done\n "
     in 
       lMatch andalso r
     end

  (* note: assuming b is an identity for f *)
  fun scan (f: 'a * 'a -> 'a) (base: 'a) (s: 'a seq) : 'a seq * 'a  =
    case length s of
      0 => (empty (), base)
    | 1 => (singleton base, nth s 0)
    | n =>
        let
          fun contract i =
            if i = n div 2 then nth s (2*i)  (* Copies the odd element at the end *)
            else f (nth s (2*i), nth s (2*i+1))

          val (r, res) = scan f base (tabulate contract ((n+1) div 2))

          fun expand i =
            if i mod 2 = 0 then nth r (i div 2)
            else f (nth r (i div 2), nth s (i-1))
        in 
          (tabulate expand n, res)
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

  fun flatten (ss as (b, e, _)) =
    let
      val _ = dPrint ("flatten\n")
      val (starts, n: int) = scan op+ 0 (map length ss)
      val _ = dPrint ("flatten, n = " ^ Int.toString n ^ "\n")
    in
      if (n = 0) then 
        empty ()
      else 
        let
          val resultArray = allocArray n

          fun copy i = 
            let 
              val s = nth ss i
            in 
              if length s = 0 then 
                ()
              else 
                let 
                  val _ = dPrint ("copy: i = " ^ Int.toString i ^ "\n")
                  val b = nth starts i
                  val _ = dPrint ("copy: b = " ^ Int.toString b ^ "\n")
                  val e = b + length(s) - 1
                  val _ = dPrint ("copy: length = " ^ Int.toString (length s) ^ "\n")
                  val _ = dPrint ("copy: e = " ^ Int.toString e ^ "\n")
             
                in
                  initialize (fn j => nth s j) (b, e, resultArray) 
                end
            end

           val _ = dPrint ("flatten: copying...\n")
           val _ =  parallelFor copy 0 (e-b)
           val _ = dPrint ("flatten: copying done...\n")
         in
           (0, n-1, resultArray)
         end
      end

  fun filter p s =
    let 
      val n = length s
      val _ = dPrint ("Input = " ^ Int.toString n ^ "\n")
    in 
      if n = 0 then 
        s 
      else
        let
          fun pick x = 
            if p x then (dPrint "filter:pick: some\n"; SOME x) 
            else (dPrint "filter:pick: none\n"; NONE)
         
          val noso as (0,_,anoso) = map pick s
          fun aoToString anoso = 
            arrayToString (fn (i, x, str) => 
                             case unwrap x of 
                               NONE => ("i = " ^ Int.toString i ^ ": None, " ^ str)
                             | SOME _ => ("i = " ^ Int.toString i ^ ": Some, " ^ str)
                           )
                          "!"
                          anoso
          
          val _ = dPrint ("aoToString: " ^ (aoToString anoso) ^ "\n")
 
          val ((0,_,apositions), m) = scan op+ 0 (map (fn SOME _ => 1 | NONE => 0) noso)

(*
          val spos = 
            arrayToString (fn (i,  x, str) => Int.toString (unwrap x) ^ " " ^ str)
                          "!"
                          apositions

          val _ = print ("filter: m = " ^ (Int.toString m) ^ "\n")
          val _ = print ("filter: positions = " ^  spos ^ "\n")
*)
        in 
          if m = 0 then 
            empty ()
          else
            let 
              val resultArray = compactArray anoso  apositions m
            in
              (0, m-1, resultArray)
            end
         end
      end


  fun filterIdx p =
    map (fn (_, x) => x) o (filter p) o enum

  fun bsearch cmp s x =
    let
      val size = length s
    in
      if size = 0 then
        0
      else if size = 1 then   
        case cmp (x, nth s 0) of 
          EQUAL => 0
        | LESS => 0
        | GREATER => 1
      else
        let
          val mid = size div 2
          val pivot = nth s mid
        in
          case cmp (x, pivot) of
            EQUAL => mid
          | LESS => bsearch cmp (take s mid) x
          | GREATER => mid + (bsearch cmp (drop s mid) x)
        end
      end

  fun merge cmp (s, t) =
      case splitMid s of 
        EMPTY => t
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
