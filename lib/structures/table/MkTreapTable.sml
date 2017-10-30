functor MkTreapTable (structure Key : HASHKEY)
  :> ORDTABLE where type Key.t = Key.t
                and type 'a Seq.t = 'a ArraySequence.t =
MkBSTTable (structure BST = MkTreap (structure Key = Key)
            structure Seq = ArraySequence)
