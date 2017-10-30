signature BST =
sig
  structure Key : ORDKEY

  type 'a t
  type 'a bst = 'a t

  datatype 'a view =
    LEAF
  | NODE of { key : Key.t
            , value : 'a
            , left : 'a bst
            , right : 'a bst }

  val expose : 'a bst -> 'a view
  val size : 'a bst -> int

  val empty : unit -> 'a bst
  val singleton : Key.t * 'a -> 'a bst

  val join : 'a bst * 'a bst -> 'a bst
  val joinMid : 'a bst * (Key.t * 'a) * 'a bst -> 'a bst

  val split : 'a bst * Key.t -> 'a bst * 'a option * 'a bst

  val $ : Key.t * 'a -> 'a bst
end
