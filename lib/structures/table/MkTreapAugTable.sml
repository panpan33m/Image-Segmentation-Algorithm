functor MkTreapAugTable (structure Key : HASHKEY
                         structure Val : MONOID)
  :> AUG_ORDTABLE where type Key.t = Key.t
                    and type Val.t = Val.t
                    and type 'a Seq.t = 'a ArraySequence.t =
MkBSTAugTable (structure BST = MkAugTreap (structure Key = Key
                                           structure Val = Val)
               structure Seq = ArraySequence)
