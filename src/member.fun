functor Member(Flag : sig type t val compile : t -> Word.word end) =
  struct
    type t = {
      accessFlags : Flag.t list,
      name : Text.t,
      descriptor : Descriptor.t,
      attributes : Attr.t list
    }

    fun from t = t

    fun compile constPool { accessFlags, name, descriptor, attributes } =
      let
        open Util

        fun mask flags =
          let
            fun mask (flag, acc) = Word.orb (acc, Flag.compile flag)
          in
            List.foldl mask 0w0 flags
          end

        fun compileAttrs (attr, (bytes, constPool)) =
          let
            val (attrBytes, constPool) = Attr.compile constPool attr
          in
            (Word8Vector.concat [bytes, attrBytes], constPool)
          end

        val seed = (vec [], constPool)
        val (attrBytes, constPool) = List.foldl compileAttrs seed attributes
        val (nameIndex, constPool) = ConstPool.withUtf8 constPool name
        val (descIndex, constPool) = ConstPool.withUtf8 constPool (Descriptor.compile descriptor)
        val methodBytes = Word8Vector.concat [
          u2 (Word.toInt (mask accessFlags)),
          u2 nameIndex,
          u2 descIndex,
          u2 (List.length attributes),
          attrBytes
        ]
      in
        (methodBytes, constPool)
      end
  end
