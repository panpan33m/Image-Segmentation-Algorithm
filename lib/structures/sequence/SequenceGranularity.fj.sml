structure SequenceGranularity :> GRANULARITY_CONTROL =
struct
  val g = ref 1024

  fun get () = !g

  fun set g' =
    if g' < 1 then
    raise Fail "Sequence granularity must be at least 1"
    else (g := g')
end
