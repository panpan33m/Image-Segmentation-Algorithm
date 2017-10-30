structure GranularityControl =
struct
  (* g is the granularity *)
  val defaultGranularity = 1
  val g = ref defaultGranularity

  fun setGranularity granularity = g := granularity
end
