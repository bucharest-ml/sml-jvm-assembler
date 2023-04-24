structure List : LIST =
  struct
    open List

    type ('a, 's) stepper = {
      seed : 's,
      step : 'a * 's -> 's
    }

    fun foldMapState list { monoid, step, seed } =
      let
        fun fold (elem, (r1, state)) =
          let
            val (r2, state) = step elem state
          in
            (monoid r1 r2, state)
          end
      in
        List.foldl fold seed list
      end

    fun stepl { step, seed } = List.foldl step seed
    fun stepr { step, seed } = List.foldr step seed

    structure Op =
      struct
        infixr &

        (* https://smlnj-gforge.cs.uchicago.edu/tracker/?group_id=33&atid=215&func=detail&aid=129 *)
        fun f & g =
          fn (elem, (a, b)) => (f (elem, a), g (elem, b))

        fun <&> (f : ('a, 's) stepper, g : ('a, 't) stepper) = {
          seed = (#seed f, #seed g),
          step = #step f & #step g
        }
      end
  end
