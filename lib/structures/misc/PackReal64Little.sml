(* SML/NJ does not have PackReal64Little. *)

structure PackReal64Little
  :> PACK_REAL where type real = Real64.real =
struct
  structure UW8A = Unsafe.Word8Array
  structure UR64A = Unsafe.Real64Array
  structure W8V = Word8Vector
  structure W8VS = Word8VectorSlice
  structure W8AS = Word8ArraySlice

  type real = Real64.real
  val bytesPerElem = 8
  val isBigEndian = false

  fun toRaw r : UW8A.array =
    let
      val alloc = UR64A.create 1
      val _ = UR64A.update (alloc, 0, r)
    in
      Unsafe.cast alloc
    end

  fun fromRaw (bytes : UW8A.array) =
    UR64A.sub (Unsafe.cast bytes, 0)

  (* determine the machine endian *)
  exception UnknownEndian
  val rawE = toRaw Real64.Math.e
  val rawIsBigEndian =
    case List.tabulate (bytesPerElem, fn i => UW8A.sub(rawE, i)) of
         [0wx40, 0wx05, 0wxBF, 0wx0A, 0wx8B, 0wx14, 0wx57, 0wx69] => true
       | [0wx69, 0wx57, 0wx14, 0wx8B, 0wx0A, 0wxBF, 0wx05, 0wx40] => false
       | _ => raise UnknownEndian
  fun rawIndex i = if isBigEndian = rawIsBigEndian then i else bytesPerElem - i

  fun toBytes r =
    W8V.tabulate (bytesPerElem, fn i => UW8A.sub (toRaw r, rawIndex i))

  fun subVec (v, i) =
    let
      val slice = W8VS.slice (v, bytesPerElem * i, SOME bytesPerElem)
      val raw = UW8A.create bytesPerElem
      val _ = W8VS.appi (fn (i, b) => UW8A.update (raw, rawIndex i, b)) slice
    in
      fromRaw raw
    end

  fun fromBytes v = subVec (v, 0)

  fun subArr (arr, i) =
    let
      val slice = W8AS.slice (arr, bytesPerElem * i, SOME bytesPerElem)
      val raw = UW8A.create bytesPerElem
      val _ = W8AS.appi (fn (i, b) => UW8A.update (raw, rawIndex i, b)) slice
    in
      fromRaw raw
    end

  fun update (arr, i, r) =
    let
      val slice = W8AS.slice (arr, bytesPerElem * i, SOME bytesPerElem)
      val raw = toRaw r
    in
      W8AS.modifyi (fn (i, _) => UW8A.sub (raw, rawIndex i)) slice
    end
end
