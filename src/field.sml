structure Field =
  struct
    structure Flag =
      struct
        datatype t =
        | PUBLIC
        | PRIVATE
        | PROTECTED
        | STATIC
        | FINAL
        | VOLATILE
        | TRANSIENT
        | SYNTHETIC
        | ENUM

        fun compile flag : Word.word =
          case flag of
          | PUBLIC    => 0wx0001
          | PRIVATE   => 0wx0002
          | PROTECTED => 0wx0004
          | STATIC    => 0wx0008
          | FINAL     => 0wx0010
          | VOLATILE  => 0wx0040
          | TRANSIENT => 0wx0080
          | SYNTHETIC => 0wx1000
          | ENUM      => 0wx4000
      end

    structure M = Member(Flag) open M
  end
