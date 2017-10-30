functor MkSTSequence (structure Seq : SEQUENCE)
  :> ST_SEQUENCE where type 'a Seq.t = 'a Seq.t =
struct
  structure Seq = Seq

  datatype 'a update = UPD of int * 'a | INJ of (int * 'a) Seq.t

  (* CUR ary ==> ary is current and can be accessed/updated quickly
   * MOD ==> need to reconstruct the array from history; inefficient
   *)
  datatype 'a state = CUR of 'a array | MOD
  type 'a t = { state : 'a state ref,
                origin : 'a array,
                updates : 'a update list }

  type 'a stseq = 'a t

  exception Range

  fun copyA2A ary =
    Array.tabulate (Array.length ary, fn i => Array.sub (ary, i))

  fun copyA2S ary =
    Seq.tabulate (fn i => Array.sub (ary, i)) (Array.length ary)

  fun copyS2A seq =
    Array.tabulate (Seq.length seq, Seq.nth seq)

  fun fromSeq s =
    let
      val orig = copyS2A s
      val cur = copyA2A orig
    in {state=ref (CUR cur), origin=orig, updates=[]}
    end

  local
    fun rebuild (S as {state, origin, updates}) : 'a array =
      let
        val ary = copyA2A origin
        fun write (i,v) = Array.update (ary, i, v)
        fun apply [] = ()
          | apply (UPD (i,v)::upd) = (apply upd; write (i,v))
          | apply (INJ I::upd) =
            (apply upd; Seq.iterate (fn (_, (i,v)) => write (i,v)) () I)
      in
        (apply updates; state := CUR ary; ary)
      end
  in
    fun mkCurrent (S as {state=ref (CUR ary), ...}) = ary
      | mkCurrent S = rebuild S (* inefficient mode *)
  end

  fun toSeq S = copyA2S (mkCurrent S)

  fun nth S i = Array.sub (mkCurrent S, i) handle Subscript => raise Range

  fun modify f upd (S as {state, origin, updates}) =
    let
      val ary = mkCurrent S
      val _ = (state := MOD; f ary)
    in {state=ref (CUR ary), origin=origin, updates=upd::updates}
    end handle Subscript => raise Range

  fun app f = Seq.iterate (fn (_, x) => f x) ()
  fun write ary (i,v) = Array.update (ary, i, v)
  fun update (s, (i,v)) = modify (fn ary => write ary (i,v)) (UPD (i,v)) s
  fun inject (s, I) = modify (fn ary => app (write ary) I) (INJ I) s
end
