structure List : LIST =
  struct
    open List

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
  end
