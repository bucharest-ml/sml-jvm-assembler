signature LIST =
  sig
    include LIST

    val foldMapState :
        'a list
      -> {
        monoid : 'b -> 'b -> 'b,
        step : 'a -> 's -> ('b * 's),
        seed : 'b * 's
      }
      -> ('b * 's)
  end
