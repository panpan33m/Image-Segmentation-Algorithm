structure StatRunner :>
sig
  val run : (unit -> 'a) -> 'a * string
end =
struct

  fun run (f : unit -> 'a) : 'a * string =
    let
      val startTime = Time.now ()
      val result = f ()
      val endTime = Time.now ()

      val elapsedTime = Time.- (endTime, startTime)
    in
      (result,
       String.concat [
         "wall-elapsed: ", LargeInt.toString (Time.toMilliseconds elapsedTime), " ms\n",
       ])
    end

end
