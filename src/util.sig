signature UTIL =
  sig
    val vec : Word8.word list -> Word8Vector.vector
    val u1 : int -> Word8Vector.vector
    val u2 : int -> Word8Vector.vector
    val u4 : int -> Word8Vector.vector
  end
