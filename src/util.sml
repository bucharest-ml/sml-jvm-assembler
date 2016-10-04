structure Util :> UTIL =
  struct
    open Word infix andb >>

    val vec = Word8Vector.fromList

    fun u1 word = vec [Word8.fromInt word]

    fun u2 word =
      let
        val word = Word.fromInt word
      in
        vec [
          Word8.fromInt (Word.toInt ((word >> 0w08) andb 0wxFF)),
          Word8.fromInt (Word.toInt (word andb 0wxFF))
        ]
      end

    (*
     * TODO: Is it safe to use Word here? In SML/NJ, a word is 31 bits, so we
     * may lose 1-bit of precision. Maybe it's safe within the constraints of
     * the JVMS.
     *)
    fun u4 word =
      let
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
