structure List : LIST =
  struct
    open LIST

    fun foldl list { seed, step } = List.foldl step seed list
    fun foldr list { seed, step } = List.foldr step seed list

    fun takeWhile _ [] = []
      | takeWhile p (x :: xs) = if p x then x :: takeWhile p xs else []

    fun takeUntil p = takeWhile (not o p)

    fun countWhere predicate list =
      raise Fail "not implemented"

    fun bound xs ys a b =
      case (xs, ys) of
        (x, []) => a
      | ([], y) => b
      | (_ :: xs, _ :: ys) => recur xs ys a b

    (**
     * Returns the list with more elements.
     *)
    fun max a b = bound a b a b

    (**
     * Returns the list with fewer elements.
     *)
    fun min a b = bound a b b a
  end
