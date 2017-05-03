structure Verifier =
  struct
    open Instr StackLang

    fun verify instrs =
      let
        fun transition instr =
          case instr of
            nop => []
          | aconst_null => raise Fail "not implemented: aconst_null"
          | iconst_m1 => [Push VerificationType.Integer]
          | iconst_0 => [Push VerificationType.Integer]
          | iconst_1 => [Push VerificationType.Integer]
          | iconst_2 => [Push VerificationType.Integer]
          | iconst_3 => [Push VerificationType.Integer]
          | iconst_4 => [Push VerificationType.Integer]
          | iconst_5 => [Push VerificationType.Integer]
          | lconst_0 => raise Fail "not implemented: lconst_0"
          | lconst_1 => raise Fail "not implemented: lconst_1"
          | fconst_0 => raise Fail "not implemented: fconst_0"
          | fconst_1 => raise Fail "not implemented: fconst_1"
          | fconst_2 => raise Fail "not implemented: fconst_2"
          | dconst_0 => raise Fail "not implemented: dconst_0"
          | dconst_1 => raise Fail "not implemented: dconst_1"
          | bipush word => raise Fail "not implemented: bipush"
          | sipush short => raise Fail "not implemented: sipush"
          | ldc const => [Push (VerificationType.fromConst const)]
          | iload index => raise Fail "not implemented: iload"
          | lload index => raise Fail "not implemented: lload"
          | fload index => raise Fail "not implemented: fload"
          | dload index => raise Fail "not implemented: dload"
          | aload index => raise Fail "not implemented: aload"
          | iload_0 => [Load (0, VerificationType.Integer)]
          | iload_1 => [Load (1, VerificationType.Integer)]
          | iload_2 => [Load (2, VerificationType.Integer)]
          | iload_3 => [Load (3, VerificationType.Integer)]
          | lload_0 => raise Fail "not implemented: lload_0"
          | lload_1 => raise Fail "not implemented: lload_1"
          | lload_2 => raise Fail "not implemented: lload_2"
          | lload_3 => raise Fail "not implemented: lload_3"
          | fload_0 => raise Fail "not implemented: fload_0"
          | fload_1 => raise Fail "not implemented: fload_1"
          | fload_2 => raise Fail "not implemented: fload_2"
          | fload_3 => raise Fail "not implemented: fload_3"
          | dload_0 => raise Fail "not implemented: dload_0"
          | dload_1 => raise Fail "not implemented: dload_1"
          | dload_2 => raise Fail "not implemented: dload_2"
          | dload_3 => raise Fail "not implemented: dload_3"
          | aload_0 => [Load (0, VerificationType.Reference)]
          | aload_1 => [Load (1, VerificationType.Reference)]
          | aload_2 => [Load (2, VerificationType.Reference)]
          | aload_3 => [Load (3, VerificationType.Reference)]
          | iaload => raise Fail "not implemented: iaload"
          | laload => raise Fail "not implemented: laload"
          | faload => raise Fail "not implemented: faload"
          | daload => raise Fail "not implemented: daload"
          | aaload => raise Fail "not implemented: aaload"
          | baload => raise Fail "not implemented: baload"
          | caload => raise Fail "not implemented: caload"
          | saload => raise Fail "not implemented: saload"
          | istore index => raise Fail "not implemented: istore"
          | lstore index => raise Fail "not implemented: lstore"
          | fstore index => raise Fail "not implemented: fstore"
          | dstore index => raise Fail "not implemented: dstore"
          | astore index => raise Fail "not implemented: astore"
          | istore_0 => [Store (0, VerificationType.Integer)]
          | istore_1 => [Store (1, VerificationType.Integer)]
          | istore_2 => [Store (2, VerificationType.Integer)]
          | istore_3 => [Store (3, VerificationType.Integer)]
          | lstore_0 => raise Fail "not implemented: lstore_0"
          | lstore_1 => raise Fail "not implemented: lstore_1"
          | lstore_2 => raise Fail "not implemented: lstore_2"
          | lstore_3 => raise Fail "not implemented: lstore_3"
          | fstore_0 => raise Fail "not implemented: fstore_0"
          | fstore_1 => raise Fail "not implemented: fstore_1"
          | fstore_2 => raise Fail "not implemented: fstore_2"
          | fstore_3 => raise Fail "not implemented: fstore_3"
          | dstore_0 => raise Fail "not implemented: dstore_0"
          | dstore_1 => raise Fail "not implemented: dstore_1"
          | dstore_2 => raise Fail "not implemented: dstore_2"
          | dstore_3 => raise Fail "not implemented: dstore_3"
          | astore_0 => raise Fail "not implemented: astore_0"
          | astore_1 => raise Fail "not implemented: astore_1"
          | astore_2 => raise Fail "not implemented: astore_2"
          | astore_3 => raise Fail "not implemented: astore_3"
          | iastore => raise Fail "not implemented: iastore"
          | lastore => raise Fail "not implemented: lastore"
          | fastore => raise Fail "not implemented: fastore"
          | dastore => raise Fail "not implemented: dastore"
          | aastore => raise Fail "not implemented: aastore"
          | bastore => raise Fail "not implemented: bastore"
          | castore => raise Fail "not implemented: castore"
          | sastore => raise Fail "not implemented: sastore"
          | pop => raise Fail "not implemented: pop"
          | pop2 => raise Fail "not implemented: pop2"
          | dup => raise Fail "not implemented: dup"
          | dup_x1 => raise Fail "not implemented: dup_x1"
          | dup_x2 => raise Fail "not implemented: dup_x2"
          | dup2 => raise Fail "not implemented: dup2"
          | dup2_x1 => raise Fail "not implemented: dup2_x1"
          | dup2_x2 => raise Fail "not implemented: dup2_x2"
          | swap => raise Fail "not implemented: swap"
          | iadd => raise Fail "not implemented: iadd"
          | ladd => raise Fail "not implemented: ladd"
          | fadd => raise Fail "not implemented: fadd"
          | dadd => raise Fail "not implemented: dadd"
          | isub => raise Fail "not implemented: isub"
          | lsub => raise Fail "not implemented: lsub"
          | fsub => raise Fail "not implemented: fsub"
          | dsub => raise Fail "not implemented: dsub"
          | imul => [
              Pop VerificationType.Integer,
              Pop VerificationType.Integer,
              Push VerificationType.Integer
            ]
          | lmul => raise Fail "not implemented: lmul"
          | fmul => raise Fail "not implemented: fmul"
          | dmul => raise Fail "not implemented: dmul"
          | idiv => raise Fail "not implemented: idiv"
          | ldiv => raise Fail "not implemented: ldiv"
          | fdiv => raise Fail "not implemented: fdiv"
          | ddiv => raise Fail "not implemented: ddiv"
          | irem => raise Fail "not implemented: irem"
          | lrem => raise Fail "not implemented: lrem"
          | frem => raise Fail "not implemented: frem"
          | drem => raise Fail "not implemented: drem"
          | ineg => raise Fail "not implemented: ineg"
          | lneg => raise Fail "not implemented: lneg"
          | fneg => raise Fail "not implemented: fneg"
          | dneg => raise Fail "not implemented: dneg"
          | ishl => raise Fail "not implemented: ishl"
          | lshl => raise Fail "not implemented: lshl"
          | ishr => raise Fail "not implemented: ishr"
          | lshr => raise Fail "not implemented: lshr"
          | iushr => raise Fail "not implemented: iushr"
          | lushr => raise Fail "not implemented: lushr"
          | iand => raise Fail "not implemented: iand"
          | land => raise Fail "not implemented: land"
          | ior => raise Fail "not implemented: ior"
          | lor => raise Fail "not implemented: lor"
          | ixor => raise Fail "not implemented: ixor"
          | lxor => raise Fail "not implemented: lxor"
          | iinc (index, _) => [Local (Word8.toInt index, VerificationType.Integer)]
          | i2l => raise Fail "not implemented: i2l"
          | i2f => raise Fail "not implemented: i2f"
          | i2d => raise Fail "not implemented: i2d"
          | l2i => raise Fail "not implemented: l2i"
          | l2f => raise Fail "not implemented: l2f"
          | l2d => raise Fail "not implemented: l2d"
          | f2i => raise Fail "not implemented: f2i"
          | f2l => raise Fail "not implemented: f2l"
          | f2d => raise Fail "not implemented: f2d"
          | d2i => raise Fail "not implemented: d2i"
          | d2l => raise Fail "not implemented: d2l"
          | d2f => raise Fail "not implemented: d2f"
          | i2b => raise Fail "not implemented: i2b"
          | i2c => raise Fail "not implemented: i2c"
          | i2s => raise Fail "not implemented: i2s"
          | lcmp => raise Fail "not implemented: lcmp"
          | fcmpl => raise Fail "not implemented: fcmpl"
          | fcmpg => raise Fail "not implemented: fcmpg"
          | dcmpl => raise Fail "not implemented: dcmpl"
          | dcmpg => raise Fail "not implemented: dcmpg"
          | ifeq offset => raise Fail "not implemented: ifeq"
          | ifne offset => raise Fail "not implemented: ifne"
          | iflt offset => raise Fail "not implemented: iflt"
          | ifge offset => raise Fail "not implemented: ifge"
          | ifgt offset => raise Fail "not implemented: ifgt"
          | ifle offset => [
              Pop VerificationType.Integer,
              Branch { targetOffset = offset }
            ]
          | if_icmpeq offset => raise Fail "not implemented: if_icmpeq"
          | if_icmpne offset => [
              Pop VerificationType.Integer,
              Pop VerificationType.Integer,
              Branch { targetOffset = offset }
            ]
          | if_icmplt offset => raise Fail "not implemented: if_icmplt"
          | if_icmpge offset => raise Fail "not implemented: if_icmpge"
          | if_icmpgt offset => raise Fail "not implemented: if_icmpgt"
          | if_icmple offset => raise Fail "not implemented: if_icmple"
          | if_acmpeq offset => raise Fail "not implemented: if_acmpeq"
          | if_acmpne offset => raise Fail "not implemented: if_acmpne"
          | getstatic { descriptor, ... } => [Push (VerificationType.fromDescriptor descriptor)]
          | putstatic _ => raise Fail "not implemented: putstatic"
          | getfield _ => raise Fail "not implemented: getfield"
          | putfield _ => raise Fail "not implemented: putfield"
          | invokevirtual { descriptor, class, ... } =>
            let
              val paramTypes = List.revMap Pop (VerificationType.methodParams descriptor)
              val thisType = [Pop (VerificationType.Object class)]
              val returnType =
                case VerificationType.methodReturn descriptor of
                  VerificationType.Top => []
                | verificationType => [Push verificationType]
            in
              List.concat [paramTypes, thisType, returnType]
            end
          | invokespecial _ => raise Fail "not implemented: invokespecial"
          | invokestatic { descriptor, class, ... } =>
            let
              val paramTypes = List.revMap Pop (VerificationType.methodParams descriptor)
              val returnType =
                case VerificationType.methodReturn descriptor of
                  VerificationType.Top => []
                | verificationType => [Push verificationType]
            in
              List.concat [paramTypes, returnType]
            end
          | invokeinterface _ => raise Fail "not implemented: invokeinterface"
          | invokedynamic _ => raise Fail "not implemented: invokedynamic"
          | new className => raise Fail "not implemented: new"
          | newarray _ => raise Fail "not implemented: newarray"
          | anewarray index => raise Fail "not implemented: anewarray"
          | arraylength => [
              Pop (VerificationType.Array VerificationType.Top),
              Push VerificationType.Integer
            ]
          | athrow => raise Fail "not implemented: athrow"
          | checkcast index => raise Fail "not implemented: checkcast"
          | instanceof index => raise Fail "not implemented: instanceof"
          | monitorenter => raise Fail "not implemented: monitorenter"
          | monitorexit => raise Fail "not implemented: monitorexit"
          | goto offset => [Branch { targetOffset = offset }]
          | jsr offset => raise Fail "not implemented: jsr"
          | ret index => raise Fail "not implemented: ret"
          | tableswitch => raise Fail "not implemented: tableswitch"
          | lookupswitch => raise Fail "not implemented: lookupswitch"
          | ireturn => raise Fail "not implemented: ireturn"
          | lreturn => raise Fail "not implemented: lreturn"
          | freturn => raise Fail "not implemented: freturn"
          | dreturn => raise Fail "not implemented: dreturn"
          | areturn => raise Fail "not implemented: areturn"
          | return => [Push VerificationType.Top]
          | wide => raise Fail "not implemented: wide"
          | multianewarray _ => raise Fail "not implemented: multianewarray"
          | ifnull offset => raise Fail "not implemented: ifnull"
          | ifnonnull offset => raise Fail "not implemented: ifnonnull"
          | goto_w offset => raise Fail "not implemented: goto_w"
          | jsr_w offset => raise Fail "not implemented: jsr_w"
          | breakpoint => raise Fail "not implemented: breakpoint"
          | impdep1 => raise Fail "not implemented: impdep1"
          | impdep2 => raise Fail "not implemented: impdep2"
      in
        List.map
          (fn (offset, instr) =>
            { offset = offset, instrs = transition instr })
          instrs
      end
  end
