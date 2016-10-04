structure Util =
  struct
    val vec = Word8Vector.fromList

    fun u1 word = vec [Word8.fromInt word]

    fun u2 word =
      let
        open Word infix andb >>
        val word = Word.fromInt word
      in
        vec [
          Word8.fromInt (Word.toInt ((word >> 0w08) andb 0wxFF)),
          Word8.fromInt (Word.toInt (word andb 0wxFF))
        ]
      end

    fun u4 word =
      let
        open Word infix andb >>
        val word = Word.fromInt word
      in
        vec [
          Word8.fromInt (Word.toInt ((word >> 0w24) andb 0wxFF)),
          Word8.fromInt (Word.toInt ((word >> 0w16) andb 0wxFF)),
          Word8.fromInt (Word.toInt ((word >> 0w08) andb 0wxFF)),
          Word8.fromInt (Word.toInt (word andb 0wxFF))
        ]
      end
  end
