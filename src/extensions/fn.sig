signature FN =
  sig
    include FN

    (**
     * Like `flip`, but for curried functions.
     *)
    val swap : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  end
