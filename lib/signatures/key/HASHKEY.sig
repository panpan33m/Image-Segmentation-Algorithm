signature HASHKEY =
sig
  include ORDKEY
  val hash : t -> int
end
