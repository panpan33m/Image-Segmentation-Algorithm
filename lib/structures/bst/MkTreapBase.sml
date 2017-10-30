(* MkTreapBase code is shared between MkTreap and MkAugTreap.
 * A base treap has two values associated with every key: one is polymorphic,
 * the other is fixed and reduced. We use the former to implement MkTreap and
 * the latter to implement MkAugTreap. *)
functor MkTreapBase
  (structure Key : HASHKEY
   structure Val : MONOID) =
struct
  structure Key = Key
  structure Val = Val

  datatype 'a t =
    Leaf
  | Node of { data : { key : Key.t
                     , value : 'a
                     , rvalue : Val.t
                     , pri : int }
            , size : int
            , reduced : Val.t
            , left : 'a t
            , right : 'a t }

  type 'a bst = 'a t

  datatype 'a view =
    LEAF
  | NODE of { key : Key.t
            , value : 'a
            , rvalue : Val.t
            , left : 'a bst
            , right : 'a bst }

  fun reduceVal t =
    case t of
      Leaf => Val.I
    | Node {reduced, ...} => reduced

  fun size t =
    case t of
      Leaf => 0
    | Node {size, ...} => size

  fun makeNode (l, data, r) =
    Node { data = data
         , size = size l + 1 + size r
         , reduced = Val.f (reduceVal l, Val.f (#rvalue data, reduceVal r))
         , left = l
         , right = r }

  fun empty () = Leaf

  fun singleton (k, v, rv) =
    makeNode (Leaf, {key = k, value = v, rvalue = rv, pri = Key.hash k}, Leaf)

  fun expose (t : 'a bst) =
    case t of
      Leaf => LEAF
    | Node {data = {key, value, rvalue, ...}, left, right, ...} =>
        NODE {key = key, value = value, rvalue = rvalue, left = left, right = right}

  fun join (t1, t2) =
    case (t1, t2) of
      (Leaf, _) => t2
    | (_, Leaf) => t1
    | (Node {data=data1, left=l1, right=r1, ...},
       Node {data=data2, left=l2, right=r2, ...}) =>
        if #pri data1 > #pri data2
        then makeNode (l1, data1, join (r1, t2))
        else makeNode (join (t1, l2), data2, r2)

  fun split (t, k) =
    case t of
      Leaf => (Leaf, NONE, Leaf)
    | Node {data, left, right, ...} =>
        case Key.compare (k, #key data) of
          LESS =>
            let val (leftl, b, leftr) = split (left, k)
            in (leftl, b, makeNode (leftr, data, right))
            end
        | EQUAL => (left, SOME (#value data, #rvalue data), right)
        | GREATER =>
            let val (rightl, b, rightr) = split (right, k)
            in (makeNode (left, data, rightl), b, rightr)
            end

  val $ = singleton
end
