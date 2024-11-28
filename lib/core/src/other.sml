signature FOLDABLE =
  sig
    type 'a t
    type ('a, 'b) arrow = 'a -> 'b
    type 'a monoid = {
      zero : 'a,
      plus : 'a * 'a -> 'a
    }

    val foldMap : 'm monoid -> ('a -> 'm) -> 'a t -> 'm
  end
