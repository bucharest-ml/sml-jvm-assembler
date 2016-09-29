structure Integer =
  struct
    type t = int
    val compare = Int.compare
  end

structure Long =
  struct
    type t = int
    val compare = Int.compare
  end

structure Float =
  struct
    type t = real
    val compare = Real.compare
  end

structure Double =
  struct
    type t = real
    val compare = Real.compare
  end
