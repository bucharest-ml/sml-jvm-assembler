structure Frame =
  struct
    datatype diff =
    | Same
    | Full
    | Chop of int
    | Append of int

    fun localsDifference xs ys =
      let
        fun chop n xs ys =
          if n = 3
          then
            case (xs, ys) of
              ([], []) => Chop n
            | _ => Full
          else
            case (xs, ys) of
              ([], []) => Chop n
            | (_ :: xs, []) => chop (n + 1) xs []
            | (_ :: xs, VerificationType.Top :: ys) => chop (n + 1) xs ys
            | _ => Full

        fun append n xs ys =
          if n = 3
          then
            case (xs, ys) of
            | ([], []) => Append n
            | _ => Full
          else
            case (xs, ys) of
            | ([], []) => Append n
            | ([], _ :: ys) => append (n + 1) [] ys
            | (VerificationType.Top :: xs, _ :: ys) => append (n + 1) xs ys
            | _ => Full

        fun same xs ys =
          case (xs, ys) of
          | ([], []) => Same
          | ([], _ :: ys) => append 1 [] ys
          | (_ :: xs, []) => chop 1 xs []
          | (x :: xs, y :: ys) =>
              if x = y
              then same xs ys
              else
                case (x, y) of
                | (VerificationType.Top, _) => append 1 xs ys
                | (_, VerificationType.Top) => chop 1 xs ys
                | _ => Full
      in
        same xs ys
      end
  end
