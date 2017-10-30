functor MkAugTreap
  (structure Key : HASHKEY
   structure Val : MONOID)
  :> AUG_BST where type Key.t = Key.t and type Val.t = Val.t =
struct
  structure Base = MkTreapBase (structure Key = Key structure Val = Val)
  structure Key = Key
  structure Val = Val

  type t = unit Base.bst
  type bst = t

  datatype view =
    LEAF
  | NODE of { key : Key.t
            , value : Val.t
            , left : bst
            , right : bst }

  fun expose t =
    case Base.expose t of
      Base.LEAF => LEAF
    | Base.NODE {key, rvalue, left, right, ...} =>
        NODE {key = key, value = rvalue, left = left, right = right}

  val size = Base.size
  val empty = Base.empty
  val reduceVal = Base.reduceVal

  fun singleton (k, v) = Base.singleton (k, (), v)
  val $ = singleton

  val join = Base.join
  fun joinMid (t1, (k, v), t2) = join (t1, join ($ (k, v), t2))

  fun split (t, k) =
    let val (l, opt, r) = Base.split (t, k)
    in (l, Option.map (fn (_, v) => v) opt, r)
    end
end
