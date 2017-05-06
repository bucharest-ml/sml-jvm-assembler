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
        structure & =
          struct
            fun & (f, g) =
              fn (elem, (a, b)) => (f (elem, a), g (elem, b))
          end

        structure <&> =
          struct
            open & infixr &

            fun <&> (f : ('a, 's) stepper, g : ('a, 't) stepper) = {
              step = #step f & #step g,
              seed = (#seed f, #seed g)
            }
          end
      end
  end
