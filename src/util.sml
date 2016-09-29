structure Util =
  struct
    val vec = Word8Vector.fromList

    fun u2 word =
      let
        open Word32 infix andb >>
        val word = Word32.fromInt word
      in
        vec [
          Word8.fromLarge ((word >> 0w08) andb 0wxFF),
          Word8.fromLarge (word andb 0wxFF)
        ]
      end

    fun u4 word =
      let
        open Word32 infix andb >>
        val word = Word32.fromInt word
      in
        vec [
          Word8.fromLarge ((word >> 0w24) andb 0wxFF),
          Word8.fromLarge ((word >> 0w16) andb 0wxFF),
          Word8.fromLarge ((word >> 0w08) andb 0wxFF),
          Word8.fromLarge (word andb 0wxFF)
        ]
      end
  end
