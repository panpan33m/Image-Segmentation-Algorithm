structure SegTester :
sig
  val makeSegsFile : string * string * int -> unit
  val makeSegsDisplay : string * int -> unit
end = struct
  structure Seq = ArraySequence
  open Seq
  structure ST = MkSTSequence(structure Seq = Seq)
           
  structure Rand = DotMix

  structure Segmenter : SEGMENTER =
    MkBoruvkaSegmenter(structure Seq = Seq structure Rand = Rand)

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  fun connectedComponents n edges =
    let
        val revEdges = map (fn (u,v,_) => (v,u)) edges
        val selfLoops = Seq.tabulate (fn i => (i,i)) n
        fun app a b = append (a,b)
        val G = (map #2 o collect Int.compare o app revEdges o app selfLoops o map (fn (u,v,_) => (u,v))) edges
                                                                                           
        fun dfs r p (X,v) = case ST.nth X v of
          SOME _ => X
        | NONE =>
            let val X' = ST.update (X,(v,SOME(r)))
                val NG = filter (fn u => u <> v andalso u <> p) (nth G v)
                val X'' =  iterate (dfs r p) X' NG
            in  X'' end
                
        val base : (int option) ST.t = ST.fromSeq (Seq.tabulate (fn _ => NONE) n)
        val vertices = Seq.tabulate (fn i => i) n
    in
      (map Option.valOf o ST.toSeq o iterate (fn (X,v) => dfs v v (X,v)) base) vertices
    end
        
  fun segment (img as {width, height, ...}, icredit) =
      if (icredit <= 0)
      then img
      else let
        val graph = ImageGraph.genGraph4 img
        val n = width * height
        val edges = Segmenter.segment icredit (graph, n)
        val c = connectedComponents n edges
        val reconstructed = ForestImage.genImage img c
      in
        {width=width, height=height, data=reconstructed}
      end

  fun makeSegsFile (inFile, outFile, k) =
      let
        val imageIn = ImageIO.fromFile inFile
        val imageOut = segment (imageIn, k)
      in
        ImageIO.toFile (outFile, imageOut)
      end
          
  (* requires a .jpg image*)
  fun makeSegsDisplay (inFile, k) =
      let
        val imageIn = ImageIO.fromFile inFile
        val imageOut = segment (imageIn, k)
      in
        ImageIO.displayImage imageOut
      end
end
