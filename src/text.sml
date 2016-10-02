structure Text : TEXT =
  struct
    open String

    type t = string

    fun compile string = Byte.stringToBytes string
  end
