signature TEXT =
  sig
    include STRING

    type t = string

    val compile : t -> Word8Vector.vector
  end
