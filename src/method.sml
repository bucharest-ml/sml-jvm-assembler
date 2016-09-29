structure Method =
  struct
    structure Flag =
      struct
        datatype t =
          PUBLIC
        | PRIVATE
        | PROTECTED
        | STATIC
        | FINAL
        | SYNCHRONIZED
        | BRIDGE
        | VARARGS
        | NATIVE
        | ABSTRACT
        | STRICT
        | SYNTHETIC

        fun compile flag : Word.word =
          case flag of
            PUBLIC       => 0wx0001
          | PRIVATE      => 0wx0002
          | PROTECTED    => 0wx0004
          | STATIC       => 0wx0008
          | FINAL        => 0wx0010
          | SYNCHRONIZED => 0wx0020
          | BRIDGE       => 0wx0040
          | VARARGS      => 0wx0080
          | NATIVE       => 0wx0100
          | ABSTRACT     => 0wx0400
          | STRICT       => 0wx0800
          | SYNTHETIC    => 0wx1000
      end

    structure M = Member(Flag) open M
  end
