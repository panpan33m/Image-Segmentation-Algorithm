structure StatRunner :>
sig
  val run : (unit -> 'a) -> 'a * string
end =
struct

  fun run (f : unit -> 'a) : 'a * string =
    let
      val gcStartTime = MLton.GC.Statistics.time ()
      val startTime = Time.now ()

      val result = f ()

      val endTime = Time.now ()
      val gcEndTime = MLton.GC.Statistics.time ()

      val elapsedTime = Time.- (endTime, startTime)
      val elapsedGCTime = Time.- (gcEndTime, gcStartTime)
      val optimisticTime = Time.- (elapsedTime, elapsedGCTime)
    in
      (result,
       String.concat [
         "wall-elapsed ", Time.fmt 4 elapsedTime, "\n",
         "gc-elapsed ", Time.fmt 4 elapsedGCTime, "\n",
         "exectime ", Time.fmt 4 optimisticTime, "\n"
       ])
    end

end
