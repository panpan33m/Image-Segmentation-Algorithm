(**
 * 210lib/TreeSequence_G.sml
 *
 * Implements SEQUENCE_G as a binary tree
 *
 * NOTE: since this is a sequential version, changing granularity does not do
 * anything.
 *)
structure TreeSequenceG :> SEQUENCE_G =
struct
  open GranularityControl
  open TreeSequence
end
