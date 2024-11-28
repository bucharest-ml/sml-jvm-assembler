signature LIST =
  sig
    include LIST

    val countWhere : ('a -> bool) -> 'a list -> int

    val foldl : 'a list -> { seed : 'b, step : 'a * 'b -> 'b } -> 'b
  end
