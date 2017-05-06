signature LIST =
  sig
    include LIST

    type ('a, 's) stepper = {
      seed : 's,
      step : 'a * 's -> 's
    }

    val foldMapState :
        'a list
      -> {
        monoid : 'b -> 'b -> 'b,
        step : 'a -> 's -> ('b * 's),
        seed : 'b * 's
      }
      -> ('b * 's)

    val stepl : ('a, 's) stepper -> 'a list -> 's
    val stepr : ('a, 's) stepper -> 'a list -> 's

    structure Op :
      sig
        (**
         * Operator for composing fold functions.
         *)
        structure & :
          sig
            val & :
                ('a * 'state_1 -> 'state_1) * ('a * 'state_2 -> 'state_2)
              -> 'a * ('state_1 * 'state_2)
              -> 'state_1 * 'state_2
          end

        (**
         * Operator for composing `stepper`s.
         *)
        structure <&> :
          sig
            val <&> : ('a, 'b) stepper * ('a, 'c) stepper -> ('a, 'b * 'c) stepper
          end
      end
  end
