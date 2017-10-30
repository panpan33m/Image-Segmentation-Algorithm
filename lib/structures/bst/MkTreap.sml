functor MkTreap (structure Key : HASHKEY)
  :> BST where type Key.t = Key.t =
struct
  structure Trivial : MONOID where type t = unit =
  struct
    type t = unit
    val I = ()
    fun f _ = ()
    fun toString _ = "()"
  end

  structure Base = MkTreapBase (structure Key = Key
                                structure Val = Trivial)
  structure Key = Key

  type 'a t = 'a Base.bst
  type 'a bst = 'a t

  datatype 'a view =
    LEAF
  | NODE of { key : Key.t
            , value : 'a
            , left : 'a bst
            , right : 'a bst }

  fun expose t =
    case Base.expose t of
      Base.LEAF => LEAF
    | Base.NODE {key, value, left, right, ...} =>
        NODE {key = key, value = value, left = left, right = right}

  val size = Base.size
  val empty = Base.empty

  fun singleton (k, v) = Base.singleton (k, v, ())
  val $ = singleton

  val join = Base.join
  fun joinMid (t1, (k, v), t2) = join (t1, join ($ (k, v), t2))

  fun split (t, k) =
    let val (l, opt, r) = Base.split (t, k)
    in (l, Option.map (fn (v, _) => v) opt, r)
    end
end
