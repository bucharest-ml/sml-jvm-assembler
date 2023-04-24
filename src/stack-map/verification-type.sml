structure VerificationType =
  struct
    open Util

    (* See: JVMS18 / $4.10.1.2 / Verification Type System *)
    datatype t =
      Top
    | Integer
    | Float
    | Long
    | Double
    | Null
    | Array of t
    | Object of ClassName.t
    | Uninitialized of Instr.offset
    | UninitializedThis
    | Reference

    fun toString t =
      case t of
      | Top => "Top"
      | Integer => "Integer"
      | Float => "Float"
      | Long => "Long"
      | Double => "Double"
      | Null => "Null"
      | Array _ => "Array"
      | Object _ => "Object"
      | Uninitialized _ => "Uninitialized"
      | UninitializedThis => "UninitializedThis"
      | Reference => "Reference"

    (**
     * See: Java Bytecode Verification — An Overview, p7
     *)
    fun leastUpperBound a b =
      case (a, b) of
      | (Top, _) => Top
      | (_, Top) => Top
      | (a, b) =>
          if a = b
          then a
          else
            (* TODO *)
            raise Fail ("not implemented: leastUpperBound: " ^ toString a ^ " =/= " ^ toString b)

    fun isTop Top = true
      | isTop _ = false

    fun fromSimple simple =
      case simple of
      | Descriptor.Bool => Integer
      | Descriptor.Byte => Integer
      | Descriptor.Char => Integer
      | Descriptor.Double => Double
      | Descriptor.Float => Float
      | Descriptor.Int => Integer
      | Descriptor.Long => Long
      | Descriptor.Short => Integer
      | Descriptor.Object class => Object class
      | Descriptor.Array elem => Array (fromSimple elem)

    fun methodReturn descriptor =
      case descriptor of
      | Descriptor.Method { return = Descriptor.Void, ... } => Top
      | Descriptor.Method { return = Descriptor.Type simple, ... } => fromSimple simple
      | _ => raise Fail "illegal: descriptor is not a method"

    fun methodParams descriptor =
      case descriptor of
      | Descriptor.Method { params, ... } => List.map fromSimple params
      | _ => raise Fail "illegal: descriptor is not a method"

    fun fromDescriptor descriptor =
      case descriptor of
      | Descriptor.Raw text => raise Fail "not implemented"
      | Descriptor.Method { params, return } => raise Fail "not implemented"
      | Descriptor.Field simple => fromSimple simple

    fun fromConst const =
      case const of
      | Const.Integer _ => Integer
      | Const.Float _ => Float
      | Const.Long _ => Long
      | Const.Double _ => Double
      | Const.String _ => Object (ClassName.fromParts ["java", "lang", "String"])
      | Const.Class _ => Object (ClassName.fromParts ["java", "lang", "Class"])
      | Const.MethodType _ => Object (ClassName.fromParts ["java", "lang", "invoke", "MethodType"])
      | Const.MethodHandle _ => Object (ClassName.fromParts ["java", "lang", "invoke", "MethodHandle"])

    fun compile constPool vtype =
      case vtype of
      | Top                  => (u1 0, constPool)
      | Integer              => (u1 1, constPool)
      | Float                => (u1 2, constPool)
      | Long                 => (u1 4, constPool)
      | Double               => (u1 3, constPool)
      | Null                 => (u1 5, constPool)
      | Array t              => raise Fail "not implemented"
      | Object t             => raise Fail "not implemented"
      | Uninitialized offset => raise Fail "not implemented"
      | UninitializedThis    => (u1 6, constPool)
      | Reference            => raise Fail "not implemented"

    fun compileList constPool vtypes =
      List.foldMapState vtypes {
        monoid = Word8Vector.join,
        step = Fn.swap compile,
        seed = (vec [], constPool)
      }
  end
