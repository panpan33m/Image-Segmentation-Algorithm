(* Write anything you want in the structure below. To use it at the REPL, do
 * - CM.make "sandbox.cm"; open Sandbox; *)
structure Sandbox =
struct
  structure Seq = ArraySequence
  structure Segmenter = MkBoruvkaSegmenter(structure Rand = DotMix)

  fun mstToString edges =
    Seq.toString (fn (u,v,w) => "(" ^ Int.toString u ^ ","
                                ^ Int.toString v ^ ","
                                ^ Int.toString w ^ ")") edges

  local open Segmenter in
  fun runInfCredits edges n =
    let
        fun undirect E =
          Seq.flatten (Seq.map (fn (u,v,w) => Seq.% [(u,v,w),(v,u,w)]) E)
    in
        segment (Option.valOf Int.maxInt) (undirect (Seq.% edges),n)
    end
  end

  fun example() =
    let
        val _ = print (mstToString (runInfCredits ([(0,2,10), (1,0,5)]) 3)^"\n")
    in
        ()
    end
                                        
end
