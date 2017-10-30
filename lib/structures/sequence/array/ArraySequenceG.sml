(**
 * 210lib/ArraySequence_G.sml
 *
 * Implements SEQUENCE_G with
 *   type 'a seq = 'a ArraySlice.slice
 *
 * NOTE: since this is a sequential version, changing granularity does not do
 * anything.
 *)
structure ArraySequenceG :> SEQUENCE_G =
struct
  open GranularityControl
  open ArraySequence
end
