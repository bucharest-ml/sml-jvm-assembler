structure Integer =
  struct
    type t = Int32.int
    val compare = Int32.compare
  end

structure Long =
  struct
    type t = Int64.int
    val compare = Int64.compare
  end

structure Float =
  struct
    type t = Real.real
    val compare = Real.compare
  end

structure Double =
  struct
    type t = Real64.real
    val compare = Real64.compare
  end
