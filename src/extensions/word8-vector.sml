structure Word8Vector =
  struct
    open Word8Vector

    fun join a b = concat [a, b]
  end
