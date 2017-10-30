(* This is required to guarantee that multiple separate structures agree upon
 * a single hidden type. *)
structure ArraySequences :>
sig
  type 'a t
  structure Parallel : SEQUENCE_EXTRA where type 'a t = 'a t
  structure Serial : SEQUENCE where type 'a t = 'a t
end =
struct
  type 'a t = 'a ArraySlice.slice

  structure Parallel :> SEQUENCE_EXTRA where type 'a t = 'a t
    = TransparentArraySequence

  structure Serial :> SEQUENCE where type 'a t = 'a t
    = TransparentSerialArraySequence
end

structure ArraySequence = ArraySequences.Parallel
structure SerialArraySequence = ArraySequences.Serial
