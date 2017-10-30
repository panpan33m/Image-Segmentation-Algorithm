signature AUG_BST =
sig
  structure Key : ORDKEY
  structure Val : MONOID

  type t
  type bst = t

  datatype view =
    LEAF
  | NODE of { key : Key.t
            , value : Val.t
            , left : bst
            , right : bst }

  val expose : bst -> view
  val size : bst -> int
  val reduceVal : bst -> Val.t

  val empty : unit -> bst
  val singleton : Key.t * Val.t -> bst

  val join : bst * bst -> bst
  val joinMid : bst * (Key.t * Val.t) * bst -> bst

  val split : bst * Key.t -> bst * Val.t option * bst

  val $ : Key.t * Val.t -> bst
end
