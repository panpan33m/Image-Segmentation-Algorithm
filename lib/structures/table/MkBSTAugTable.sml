(* TODO: reduce duplicated code between this structure and MkBSTTable. Look at
 * MkTreap.sml for some inspiration... *)
functor MkBSTAugTable
  (structure BST : AUG_BST
   structure Seq : SEQUENCE)
  :> AUG_ORDTABLE where type Key.t = BST.Key.t
                    and type Val.t = BST.Val.t
                    and type 'a Seq.t = 'a Seq.t =
struct
  structure Key = BST.Key
  structure Val = BST.Val
  structure Seq = Seq

  structure Base =
  struct
    open BST

    fun find t k =
      case expose t of
        LEAF => NONE
      | NODE {key, value, left, right} =>
          case Key.compare (k, key) of
            LESS => find left k
          | EQUAL => SOME value
          | GREATER => find right k

    fun insertWith f (t, (k, v)) =
      case split (t, k) of
        (left, NONE   , right) => joinMid (left, (k, v), right)
      | (left, SOME v', right) => joinMid (left, (k, f (v', v)), right)

    fun insert (t, (k, v)) = insertWith #2 (t, (k, v))

    fun delete (t, k) =
      let val (left, _, right) = split (t, k) in join (left, right) end

    (* mapfilter : (Key.t * 'a -> 'b option) -> 'a bst -> 'b bst
     * simultaneously apply a map and a filter: if p (k, v) returns NONE, then
     * remove k. If p returns SOME v', then replace (k -> v) with (k -> v') *)
    fun mapfilter p t =
      case expose t of
        LEAF => empty ()
      | NODE {key, value, left, right} =>
          let val (left', right', vOpt) =
            Primitives.par3 (fn () => mapfilter p left,
                             fn () => mapfilter p right,
                             fn () => p (key, value))
          in
            case vOpt of
              NONE => join (left', right')
            | SOME value' => joinMid (left', (key, value'), right')
          end

    (* combine takes a function (f : 'a option * 'b option -> 'c option)
     * and for every k, if (f (find t1 k, find t2 k) ==> SOME v') then
     * the resulting 'c bst will contain the mapping k -> v'.
     *)
    fun combine f (t1, t2) =
      case expose t1 of
        LEAF => mapfilter (fn (_, v) => f (NONE, SOME v)) t2
      | NODE {key=k1, value=v1, left=l1, right=r1} =>
          let
            val (l2, v2Opt, r2) = split (t2, k1)
            val (left, right, vOpt) =
              Primitives.par3 (fn () => combine f (l1, l2),
                               fn () => combine f (r1, r2),
                               fn () => f (SOME v1, v2Opt))
          in
            case vOpt of
              NONE => join (left, right)
            | SOME v => joinMid (left, (k1, v), right)
          end

    fun union f (t1, t2) =
      let fun f' (NONE, y) = y
            | f' (x, NONE) = x
            | f' (SOME a, SOME b) = SOME (f (a, b))
      in combine f' (t1, t2)
      end

    fun intersection f (t1, t2) =
      let fun f' (NONE, _) = NONE
            | f' (_, NONE) = NONE
            | f' (SOME a, SOME b) = SOME (f (a, b))
      in combine f' (t1, t2)
      end

    fun difference (t1, t2) =
      let fun f (SOME a, NONE) = SOME a
            | f _ = NONE
      in combine f (t1, t2)
      end

    fun mapKey f = mapfilter (fn (k, v) => SOME (f (k, v)))
    fun map f = mapKey (fn (_, v) => f v)
    fun filterKey p = mapfilter (fn (k, v) => if p (k, v) then SOME v else NONE)
    fun filter p = filterKey (fn (_, v) => p v)

    fun reduceKey f b t =
      let
        fun reduceKey' t =
          case expose t of
            LEAF => b
          | NODE {key, value, left, right} =>
              f (reduceKey' left, f ((key, value), reduceKey' right))
      in
        reduceKey' t
      end

    fun reduce f b t =
      let
        fun reduce' t =
          case expose t of
            LEAF => b
          | NODE {value, left, right, ...} =>
              f (reduce' left, f (value, reduce' right))
      in
        reduce' t
      end

    fun iterateKey f b t =
      case expose t of
        LEAF => b
      | NODE {key, value, left, right} =>
          iterateKey f (f (iterateKey f b left, (key, value))) right

    fun iterate f = iterateKey (fn (x, (_, v)) => f (x, v))

    fun iterateKeyPrefixes f b =
      let
        fun itr state t =
          case expose t of
            LEAF => (empty (), state)
          | NODE {key, value, left, right} =>
              let
                val (tL, sL) = itr state left
                val state' = f (sL, (key, value))
                val (tR, sR) = itr state' right
              in
                (joinMid (tL, (key, sL), tR), sR)
              end
      in
        itr b
      end

    fun iteratePrefixes f =
      iterateKeyPrefixes (fn (x, (_, v)) => f (x, v))

    fun first t =
      case expose t of
        LEAF => NONE
      | NODE {left, key, value, ...} =>
          case first left of
            NONE => SOME (key, value)
          | x => x

    fun last t =
      case expose t of
        LEAF => NONE
      | NODE {right, key, value, ...} =>
          case last right of
            NONE => SOME (key, value)
          | x => x

    fun prev (t, k) = last (#1 (split (t, k)))
    fun next (t, k) = first (#3 (split (t, k)))

    fun getRange t0 (klo, khi) =
      let val (_, voptlo, t1) = split (t0, klo)
          val t2 = case voptlo of NONE => t1 | SOME v => insert (t1, (klo, v))
          val (t3, vopthi, _) = split (t2, khi)
          val t4 = case vopthi of NONE => t3 | SOME v => insert (t3, (khi, v))
      in t4
      end

    fun rank (t, k) =
      case expose t of
        LEAF => 0
      | NODE {key, left, right, ...} =>
          case Key.compare (k, key) of
            LESS => rank (left, k)
          | EQUAL => size left
          | GREATER => size left + 1 + rank (right, k)

    fun select (t, i) =
      case expose t of
        LEAF => NONE
      | NODE {key, value, left, right} =>
          case Int.compare (i, size left) of
            LESS => select (left, i)
          | EQUAL => SOME (key, value)
          | GREATER => select (right, i - 1 - size left)

    fun splitRank (t, i) =
      case select (t, i) of
        NONE => raise Fail "splitRank index out of bounds!"
      | SOME (k, v) =>
          let val (l, _, r) = split (t, k)
          in (l, insert (r, (k, v)))
          end

    fun toSeq t =
      let
        val a = Array.array (size t, NONE)
        fun writeAll i t =
          case expose t of
            LEAF => ()
          | NODE {left, key, value, right} =>
              let val lsz = size left
              in Array.update (a, i + lsz, SOME (key, value));
                 Primitives.par (fn () => writeAll i left,
                                 fn () => writeAll (i + lsz + 1) right);
                 ()
              end
        val _ = writeAll 0 t
      in
        Seq.tabulate (fn i => Option.valOf (Array.sub (a, i))) (size t)
      end

    fun fromSeq s =
      let fun firstVal (k, vs) = (k, Seq.nth vs 0)
          val kvs = Seq.map firstVal (Seq.collect Key.compare s)
      in Seq.reduce join (empty ()) (Seq.map $ kvs)
      end

  end (* Base *)

  (* TABLE ================================================================= *)
  type t = Base.t
  type table = t

  structure Set =
  struct
    structure Key = Key
    structure Seq = Seq

    type t = Base.t (* with values always Val.I *)
    type set = t

    val size = Base.size
    val toSeq = Seq.map (fn (k, _) => k) o Base.toSeq
    fun toString s =
      let val strseq = Seq.map Key.toString (toSeq s)
      in "{" ^ String.concatWith "," (Seq.toList strseq) ^ "}"
      end

    val empty = Base.empty
    fun singleton k = Base.$ (k, Val.I)
    val $ = singleton
    val fromSeq = Base.fromSeq o Seq.map (fn k => (k, Val.I))

    fun find s k = Option.isSome (Base.find s k)
    fun insert (s, k) = Base.insert (s, (k, Val.I))
    val delete = Base.delete

    fun filterKey p = Base.filterKey (fn (k, _) => p k)

    fun reduceKey f b =
      #1 o Base.reduceKey (fn ((k1, _), (k2, _)) => (f (k1, k2), Val.I)) (b, Val.I)
    fun iterateKey f b = Base.iterateKey (fn (x, (k, _)) => f (x, k)) b

    val union = Base.union (fn _ => Val.I)
    fun intersection (s1, s2) = Base.intersection (fn _ => Val.I) (s1, s2)
    val difference = Base.difference

    (* ordered sets *)
    fun first s = Option.map #1 (Base.first s)
    fun last s = Option.map #1 (Base.last s)
    fun prev (s, k) = Option.map #1 (Base.prev (s, k))
    fun next (s, k) = Option.map #1 (Base.next (s, k))

    fun split (s, k) =
      let val (l, b, r) = Base.split (s, k) in (l, Option.isSome b, r) end
    val join = Base.join
    val getRange = Base.getRange
    val rank = Base.rank
    fun select (s, i) = Option.map #1 (Base.select (s, i))
    val splitRank = Base.splitRank

  end (* Set *)

  val size = Base.size
  fun domain t = Base.map (fn _ => Val.I) t
  fun range t = Seq.map (fn (_, v) => v) (Base.toSeq t)
  fun toString t =
    let fun kvtos (k, v) = "(" ^ Key.toString k ^ " -> " ^ Val.toString v ^ ")"
        val kvstrs = Seq.map kvtos (Base.toSeq t)
    in "{" ^ String.concatWith "," (Seq.toList kvstrs) ^ "}"
    end
  val toSeq = Base.toSeq

  val find = Base.find
  val insert = Base.insert
  val insertWith = Base.insertWith
  val delete = Base.delete

  val empty = Base.empty
  val singleton = Base.singleton
  val $ = singleton
  fun tabulate f = Base.mapKey (fn (k, _) => f k)
  (*fun collect s = Base.fromSeq (Seq.collect Key.compare s)*)
  val fromSeq = Base.fromSeq

  val map = Base.map
  val mapKey = Base.mapKey
  val filter = Base.filter
  val filterKey = Base.filterKey

  val reduce = Base.reduce
  val iterate = Base.iterate
  val iteratePrefixes = Base.iteratePrefixes

  val union = Base.union
  val intersection = Base.intersection
  val difference = Base.difference

  fun restrict (t, s) = intersection (fn (x, _) => x) (t, s)
  fun subtract (t, s) = difference (t, s)

  (* ordered tables *)
  val first = Base.first
  val last = Base.last
  val prev = Base.prev
  val next = Base.next

  val split = Base.split
  val join = Base.join
  val getRange = Base.getRange
  val rank = Base.rank
  val select = Base.select
  val splitRank = Base.splitRank

  val reduceVal = Base.reduceVal
end
