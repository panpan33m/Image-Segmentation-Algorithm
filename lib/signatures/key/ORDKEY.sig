signature ORDKEY =
sig
  include EQKEY
  val compare : t * t -> order
end
