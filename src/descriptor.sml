(**
 * https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.3
 *)
structure Descriptor =
  struct
    datatype t =
      Raw of Text.t
    | Field of simple
    | Method of { params : simple list, return : return }

    and simple =
      Bool
    | Byte
    | Char
    | Double
    | Float
    | Int
    | Long
    | Short
    | Object of ClassName.t
    | Array of simple

    and return =
      Void
    | Type of simple

    fun fromString s = Raw s

    fun paramsCount descriptor =
      case descriptor of
        Raw d => 1
      | Field _ => raise Fail "paramsCount called on field descriptor"
      | Method { params, ... } =>
        let
          fun size param =
            case param of
              Double => 2
            | Long => 2
            | _ => 1
        in
          List.foldl (op +) 0 (List.map size params)
        end

    fun returnCount descriptor =
      case descriptor of
        Method { params, return = Void } => 0
      | Method _ => 1
      | Field _ => raise Fail "returnCount called on field descriptor"
      | Raw d => 0

    fun compile descriptor =
      let
        fun simple f =
          case f of
            Byte => "B"
          | Char => "C"
          | Double => "D"
          | Float => "F"
          | Int => "I"
          | Long => "J"
          | Short => "S"
          | Bool => "Z"
          | Object className  => "L" ^ className ^ ";"
          | Array elemType => "[" ^ simple elemType
        fun return r =
          case r of
            Void => "V"
          | Type t => simple t
      in
        case descriptor of
          Raw d => d
        | Field f => simple f
        | Method { params, return = r } =>
            "(" ^ String.concat (List.map simple params) ^ ")" ^ return r
      end

    fun validate descriptor =
      (* Validate max 255 params in method descriptor. See §4.3.3 *)
      (* Validate max 255 dimensions for arrays. See §4.4.1 *)
      raise Fail "not implemented"
  end
