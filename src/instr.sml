structure Const =
  struct
    datatype t =
      Integer of Integer.t
    | Float of Float.t
    | Long of Long.t
    | Double of Double.t
    | String of Text.t
    | Class of ClassName.t
    | MethodType of Text.t
    | MethodHandle of { kind : MethodHandle.t, symbolRef : ConstPool.symbol_ref }
  end

structure ArrayType =
  struct
    datatype t =
      BOOLEAN
    | CHAR
    | FLOAT
    | DOUBLE
    | BYTE
    | SHORT
    | INT
    | LONG

    fun compile t : Word8.word =
      case t of
        BOOLEAN => 0w4
      | CHAR => 0w5
      | FLOAT => 0w6
      | DOUBLE => 0w7
      | BYTE => 0w8
      | SHORT => 0w9
      | INT => 0w10
      | LONG => 0w11
  end

structure Instr =
  struct
    open Util

    type offset = int
    type index = Word8.word

    datatype t =
      nop (* Constants *)
    | aconst_null
    | iconst_m1
    | iconst_0
    | iconst_1
    | iconst_2
    | iconst_3
    | iconst_4
    | iconst_5
    | lconst_0
    | lconst_1
    | fconst_0
    | fconst_1
    | fconst_2
    | dconst_0
    | dconst_1
    | bipush of Word8.word
    | sipush of int
    | ldc of Const.t
    | iload of index (* Loads *)
    | lload of index
    | fload of index
    | dload of index
    | aload of index
    | iload_0
    | iload_1
    | iload_2
    | iload_3
    | lload_0
    | lload_1
    | lload_2
    | lload_3
    | fload_0
    | fload_1
    | fload_2
    | fload_3
    | dload_0
    | dload_1
    | dload_2
    | dload_3
    | aload_0
    | aload_1
    | aload_2
    | aload_3
    | iaload
    | laload
    | faload
    | daload
    | aaload
    | baload
    | caload
    | saload
    | istore of index (* Stores *)
    | lstore of index
    | fstore of index
    | dstore of index
    | astore of index
    | istore_0
    | istore_1
    | istore_2
    | istore_3
    | lstore_0
    | lstore_1
    | lstore_2
    | lstore_3
    | fstore_0
    | fstore_1
    | fstore_2
    | fstore_3
    | dstore_0
    | dstore_1
    | dstore_2
    | dstore_3
    | astore_0
    | astore_1
    | astore_2
    | astore_3
    | iastore
    | lastore
    | fastore
    | dastore
    | aastore
    | bastore
    | castore
    | sastore
    | pop (* Stack *)
    | pop2
    | dup
    | dup_x1
    | dup_x2
    | dup2
    | dup2_x1
    | dup2_x2
    | swap
    | iadd (* Math *)
    | ladd
    | fadd
    | dadd
    | isub
    | lsub
    | fsub
    | dsub
    | imul
    | lmul
    | fmul
    | dmul
    | idiv
    | ldiv
    | fdiv
    | ddiv
    | irem
    | lrem
    | frem
    | drem
    | ineg
    | lneg
    | fneg
    | dneg
    | ishl
    | lshl
    | ishr
    | lshr
    | iushr
    | lushr
    | iand
    | land
    | ior
    | lor
    | ixor
    | lxor
    | iinc of index * Word8.word
    | i2l (* Conversions *)
    | i2f
    | i2d
    | l2i
    | l2f
    | l2d
    | f2i
    | f2l
    | f2d
    | d2i
    | d2l
    | d2f
    | i2b
    | i2c
    | i2s
    | lcmp (* Comparisons *)
    | fcmpl
    | fcmpg
    | dcmpl
    | dcmpg
    | ifeq of offset
    | ifne of offset
    | iflt of offset
    | ifge of offset
    | ifgt of offset
    | ifle of offset
    | if_icmpeq of offset
    | if_icmpne of offset
    | if_icmplt of offset
    | if_icmpge of offset
    | if_icmpgt of offset
    | if_icmple of offset
    | if_acmpeq of offset
    | if_acmpne of offset
    | getstatic of {
        class : ClassName.t,
        name : Text.t,
        descriptor : Descriptor.t
      }
    | putstatic of {
        class : ClassName.t,
        name : Text.t,
        descriptor : Descriptor.t
      }
    | getfield of {
        class : ClassName.t,
        name : Text.t,
        descriptor : Descriptor.t
      }
    | putfield of {
        class : ClassName.t,
        name : Text.t,
        descriptor : Descriptor.t
      }
    | invokevirtual of {
        class : ClassName.t,
        name : Text.t,
        descriptor : Descriptor.t
      }
    | invokespecial of {
        class : ClassName.t,
        name : Text.t,
        descriptor : Descriptor.t
      }
    | invokestatic of {
        class : ClassName.t,
        name : Text.t,
        descriptor : Descriptor.t
      }
    | invokeinterface of {
        class : ClassName.t,
        name : Text.t,
        descriptor : Descriptor.t
      }
    | invokedynamic of ConstPool.call_site
    | new of ClassName.t
    | newarray of ArrayType.t
    | anewarray of int
    | arraylength
    | athrow
    | checkcast of int
    | instanceof of int
    | monitorenter
    | monitorexit
    | goto of offset (* Control *)
    | jsr of offset
    | ret of index
    | tableswitch
    | lookupswitch
    | ireturn
    | lreturn
    | freturn
    | dreturn
    | areturn
    | return
    | wide (* Extended *)
    | multianewarray of int * Word8.word
    | ifnull of offset
    | ifnonnull of offset
      (*
       * We're able to represent wide offsets as ints instead of Word32.word
       * because of limitations in the JVM:
       *
       * "Although the jsr_w instruction takes a 4-byte branch offset, other
       * factors limit the size of a method to 65535 bytes (§4.11). This limit
       * may be raised in a future release of the Java Virtual Machine."
       *)
    | goto_w of offset
    | jsr_w of offset
    | breakpoint (* Reserved *)
    | impdep1
    | impdep2

    infix +: val op +: = Word8Vector.prepend
    infix :+ val op :+ = Word8Vector.append

    fun compile constPool instr =
      case instr of
        nop               => (vec [0wx0], 0, constPool)
      | aconst_null       => (vec [0wx1], 1, constPool)
      | iconst_m1         => (vec [0wx2], 1, constPool)
      | iconst_0          => (vec [0wx3], 1, constPool)
      | iconst_1          => (vec [0wx4], 1, constPool)
      | iconst_2          => (vec [0wx5], 1, constPool)
      | iconst_3          => (vec [0wx6], 1, constPool)
      | iconst_4          => (vec [0wx7], 1, constPool)
      | iconst_5          => (vec [0wx8], 1, constPool)
      | lconst_0          => (vec [0wx9], 2, constPool)
      | lconst_1          => (vec [0wxA], 2, constPool)
      | fconst_0          => (vec [0wxB], 1, constPool)
      | fconst_1          => (vec [0wxC], 1, constPool)
      | fconst_2          => (vec [0wxD], 1, constPool)
      | dconst_0          => (vec [0wxE], 2, constPool)
      | dconst_1          => (vec [0wxF], 2, constPool)
      | bipush word       => (vec [0wx10, word], 1, constPool)
      | sipush short      => (Word8Vector.prepend (0wx11, u2 short), 1, constPool)
      | ldc const =>
        let
          fun ldc addConst value =
            let
              val (index, constPool) = addConst constPool value
            in
              if index < 256
              then (0wx12 +: u1 index, 1, constPool) (* ldc *)
              else (0wx13 +: u2 index, 1, constPool) (* ldc_w *)
            end

          fun ldc2_w addConst value =
            let
              val (index, constPool) = addConst constPool value
            in
              (0wx14 +: u2 index, 2, constPool)
            end
        in
          case const of
            Const.Integer a      => ldc ConstPool.withInteger a
          | Const.Float a        => ldc ConstPool.withFloat a
          | Const.String a       => ldc ConstPool.withString a
          | Const.Class a        => ldc ConstPool.withClass a
          | Const.MethodType a   => ldc ConstPool.withMethodType a
          | Const.MethodHandle a => ldc ConstPool.withMethodHandle a
          | Const.Long a         => ldc2_w ConstPool.withLong a
          | Const.Double a       => ldc2_w ConstPool.withDouble a
        end
      | iload index       => (vec [0wx15, index], 1, constPool)
      | lload index       => (vec [0wx16, index], 2, constPool)
      | fload index       => (vec [0wx17, index], 1, constPool)
      | dload index       => (vec [0wx18, index], 2, constPool)
      | aload index       => (vec [0wx19, index], 1, constPool)
      | iload_0           => (vec [0wx1A], 1, constPool)
      | iload_1           => (vec [0wx1B], 1, constPool)
      | iload_2           => (vec [0wx1C], 1, constPool)
      | iload_3           => (vec [0wx1D], 1, constPool)
      | lload_0           => (vec [0wx1E], 2, constPool)
      | lload_1           => (vec [0wx1F], 2, constPool)
      | lload_2           => (vec [0wx20], 2, constPool)
      | lload_3           => (vec [0wx21], 2, constPool)
      | fload_0           => (vec [0wx22], 1, constPool)
      | fload_1           => (vec [0wx23], 1, constPool)
      | fload_2           => (vec [0wx24], 1, constPool)
      | fload_3           => (vec [0wx25], 1, constPool)
      | dload_0           => (vec [0wx26], 2, constPool)
      | dload_1           => (vec [0wx27], 2, constPool)
      | dload_2           => (vec [0wx28], 2, constPool)
      | dload_3           => (vec [0wx29], 2, constPool)
      | aload_0           => (vec [0wx2A], 1, constPool)
      | aload_1           => (vec [0wx2B], 1, constPool)
      | aload_2           => (vec [0wx2C], 1, constPool)
      | aload_3           => (vec [0wx2D], 1, constPool)
      | iaload            => (vec [0wx2E], 1, constPool)
      | laload            => (vec [0wx2F], 0, constPool)
      | faload            => (vec [0wx30], 1, constPool)
      | daload            => (vec [0wx31], 0, constPool)
      | aaload            => (vec [0wx32], 1, constPool)
      | baload            => (vec [0wx33], 1, constPool)
      | caload            => (vec [0wx34], 1, constPool)
      | saload            => (vec [0wx35], 1, constPool)
      | istore index      => (vec [0wx36, index], ~1, constPool)
      | lstore index      => (vec [0wx37, index], ~2, constPool)
      | fstore index      => (vec [0wx38, index], ~1, constPool)
      | dstore index      => (vec [0wx39, index], ~2, constPool)
      | astore index      => (vec [0wx3A, index], ~1, constPool)
      | istore_0          => (vec [0wx3B], ~1, constPool)
      | istore_1          => (vec [0wx3C], ~1, constPool)
      | istore_2          => (vec [0wx3D], ~1, constPool)
      | istore_3          => (vec [0wx3E], ~1, constPool)
      | lstore_0          => (vec [0wx3F], ~2, constPool)
      | lstore_1          => (vec [0wx40], ~2, constPool)
      | lstore_2          => (vec [0wx41], ~2, constPool)
      | lstore_3          => (vec [0wx42], ~2, constPool)
      | fstore_0          => (vec [0wx43], ~1, constPool)
      | fstore_1          => (vec [0wx44], ~1, constPool)
      | fstore_2          => (vec [0wx45], ~1, constPool)
      | fstore_3          => (vec [0wx46], ~1, constPool)
      | dstore_0          => (vec [0wx47], ~2, constPool)
      | dstore_1          => (vec [0wx48], ~2, constPool)
      | dstore_2          => (vec [0wx49], ~2, constPool)
      | dstore_3          => (vec [0wx4A], ~2, constPool)
      | astore_0          => (vec [0wx4B], ~1, constPool)
      | astore_1          => (vec [0wx4C], ~1, constPool)
      | astore_2          => (vec [0wx4D], ~1, constPool)
      | astore_3          => (vec [0wx4E], ~1, constPool)
      | iastore           => (vec [0wx4F], ~3, constPool)
      | lastore           => (vec [0wx50], ~4, constPool)
      | fastore           => (vec [0wx51], ~3, constPool)
      | dastore           => (vec [0wx52], ~4, constPool)
      | aastore           => (vec [0wx53], ~3, constPool)
      | bastore           => (vec [0wx54], ~3, constPool)
      | castore           => (vec [0wx55], ~3, constPool)
      | sastore           => (vec [0wx56], ~3, constPool)
      | pop               => (vec [0wx57], ~1, constPool)
      | pop2              => (vec [0wx58], ~2, constPool)
      | dup               => (vec [0wx59], 1, constPool)
      | dup_x1            => (vec [0wx5A], 1, constPool)
      | dup_x2            => (vec [0wx5B], 1, constPool)
      | dup2              => (vec [0wx5C], 2, constPool)
      | dup2_x1           => (vec [0wx5D], 2, constPool)
      | dup2_x2           => (vec [0wx5E], 2, constPool)
      | swap              => (vec [0wx5F], 0, constPool)
      | iadd              => (vec [0wx60], ~1, constPool)
      | ladd              => (vec [0wx61], ~2, constPool)
      | fadd              => (vec [0wx62], ~1, constPool)
      | dadd              => (vec [0wx63], ~2, constPool)
      | isub              => (vec [0wx64], ~1, constPool)
      | lsub              => (vec [0wx65], ~2, constPool)
      | fsub              => (vec [0wx66], ~1, constPool)
      | dsub              => (vec [0wx67], ~2, constPool)
      | imul              => (vec [0wx68], ~1, constPool)
      | lmul              => (vec [0wx69], ~2, constPool)
      | fmul              => (vec [0wx6A], ~1, constPool)
      | dmul              => (vec [0wx6B], ~2, constPool)
      | idiv              => (vec [0wx6C], ~1, constPool)
      | ldiv              => (vec [0wx6D], ~2, constPool)
      | fdiv              => (vec [0wx6E], ~1, constPool)
      | ddiv              => (vec [0wx6F], ~2, constPool)
      | irem              => (vec [0wx70], ~1, constPool)
      | lrem              => (vec [0wx71], ~2, constPool)
      | frem              => (vec [0wx72], ~1, constPool)
      | drem              => (vec [0wx73], ~2, constPool)
      | ineg              => (vec [0wx74], 0, constPool)
      | lneg              => (vec [0wx75], 0, constPool)
      | fneg              => (vec [0wx76], 0, constPool)
      | dneg              => (vec [0wx77], 0, constPool)
      | ishl              => (vec [0wx78], ~1, constPool)
      | lshl              => (vec [0wx78], ~1, constPool)
      | ishr              => (vec [0wx7A], ~1, constPool)
      | lshr              => (vec [0wx7A], ~1, constPool)
      | iushr             => (vec [0wx7C], ~1, constPool)
      | lushr             => (vec [0wx7D], ~1, constPool)
      | iand              => (vec [0wx7E], ~1, constPool)
      | land              => (vec [0wx7F], ~2, constPool)
      | ior               => (vec [0wx80], ~1, constPool)
      | lor               => (vec [0wx81], ~2, constPool)
      | ixor              => (vec [0wx82], ~1, constPool)
      | lxor              => (vec [0wx83], ~2, constPool)
      | iinc (index, inc) => (vec [0wx84, index, inc], 0, constPool)
      | i2l               => (vec [0wx85], 1, constPool)
      | i2f               => (vec [0wx86], 0, constPool)
      | i2d               => (vec [0wx87], 1, constPool)
      | l2i               => (vec [0wx88], ~1, constPool)
      | l2f               => (vec [0wx89], ~1, constPool)
      | l2d               => (vec [0wx8A], 0, constPool)
      | f2i               => (vec [0wx8B], 0, constPool)
      | f2l               => (vec [0wx8C], 1, constPool)
      | f2d               => (vec [0wx8D], 1, constPool)
      | d2i               => (vec [0wx8E], ~1, constPool)
      | d2l               => (vec [0wx8F], 0, constPool)
      | d2f               => (vec [0wx90], ~1, constPool)
      | i2b               => (vec [0wx91], 0, constPool)
      | i2c               => (vec [0wx92], 0, constPool)
      | i2s               => (vec [0wx93], 0, constPool)
      | lcmp              => (vec [0wx94], ~3, constPool)
      | fcmpl             => (vec [0wx95], ~1, constPool)
      | fcmpg             => (vec [0wx96], ~1, constPool)
      | dcmpl             => (vec [0wx97], ~3, constPool)
      | dcmpg             => (vec [0wx98], ~3, constPool)
      | ifeq offset       => (Word8Vector.prepend (0wx99, u2 offset), ~1, constPool)
      | ifne offset       => (Word8Vector.prepend (0wx9A, u2 offset), ~1, constPool)
      | iflt offset       => (Word8Vector.prepend (0wx9B, u2 offset), ~1, constPool)
      | ifge offset       => (Word8Vector.prepend (0wx9C, u2 offset), ~1, constPool)
      | ifgt offset       => (Word8Vector.prepend (0wx9D, u2 offset), ~1, constPool)
      | ifle offset       => (Word8Vector.prepend (0wx9E, u2 offset), ~1, constPool)
      | if_icmpeq offset  => (Word8Vector.prepend (0wx9F, u2 offset), ~2, constPool)
      | if_icmpne offset  => (Word8Vector.prepend (0wxA0, u2 offset), ~2, constPool)
      | if_icmplt offset  => (Word8Vector.prepend (0wxA1, u2 offset), ~2, constPool)
      | if_icmpge offset  => (Word8Vector.prepend (0wxA2, u2 offset), ~2, constPool)
      | if_icmpgt offset  => (Word8Vector.prepend (0wxA3, u2 offset), ~2, constPool)
      | if_icmple offset  => (Word8Vector.prepend (0wxA4, u2 offset), ~2, constPool)
      | if_acmpeq offset  => (Word8Vector.prepend (0wxA5, u2 offset), ~2, constPool)
      | if_acmpne offset  => (Word8Vector.prepend (0wxA6, u2 offset), ~2, constPool)
      | getstatic { class, name, descriptor } =>
        let
          val (fieldIndex, constPool) =
            ConstPool.withFieldref constPool {
              class = class,
              nameAndType = {
                name = name,
                descriptor = descriptor
              }
            }
          val bytes = Word8Vector.prepend (0wxB2, u2 fieldIndex)
        in
          (bytes, 1, constPool)
        end
      | putstatic { class, name, descriptor } =>
        let
          val (fieldIndex, constPool) =
            ConstPool.withFieldref constPool {
              class = class,
              nameAndType = {
                name = name,
                descriptor = descriptor
              }
            }
          val bytes = Word8Vector.prepend (0wxB3, u2 fieldIndex)
        in
          (bytes, ~1, constPool)
        end
      | getfield { class, name, descriptor } =>
        let
          val (fieldIndex, constPool) =
            ConstPool.withFieldref constPool {
              class = class,
              nameAndType = {
                name = name,
                descriptor = descriptor
              }
            }
          val bytes = Word8Vector.prepend (0wxB4, u2 fieldIndex)
        in
          (bytes, 0, constPool)
        end
      | putfield { class, name, descriptor } =>
        let
          val (fieldIndex, constPool) =
            ConstPool.withFieldref constPool {
              class = class,
              nameAndType = {
                name = name,
                descriptor = descriptor
              }
            }
          val bytes = Word8Vector.prepend (0wxB5, u2 fieldIndex)
        in
          (bytes, ~2, constPool)
        end
      | invokevirtual { class, name, descriptor } =>
        let
          val (methodIndex, constPool) =
            ConstPool.withMethodref constPool {
              class = class,
              nameAndType = {
                name = name,
                descriptor = descriptor
              }
            }
          val bytes = Word8Vector.prepend (0wxB6, u2 methodIndex)
          val paramsCount = Descriptor.paramsCount descriptor
          val returnCount = Descriptor.returnCount descriptor
        in
          (bytes, returnCount - paramsCount - 1 (* this *), constPool)
        end
      | invokespecial { class, name, descriptor } =>
        let
          val (methodIndex, constPool) =
            ConstPool.withMethodref constPool {
              class = class,
              nameAndType = {
                name = name,
                descriptor = descriptor
              }
            }
          val bytes = Word8Vector.prepend (0wxB7, u2 methodIndex)
          val paramsCount = Descriptor.paramsCount descriptor
          val returnCount = Descriptor.returnCount descriptor
        in
          (bytes, returnCount - paramsCount - 1 (* this *), constPool)
        end
      | invokestatic { class, name, descriptor } =>
        let
          val (methodIndex, constPool) =
            ConstPool.withMethodref constPool {
              class = class,
              nameAndType = {
                name = name,
                descriptor = descriptor
              }
            }
          val bytes = Word8Vector.prepend (0wxB8, u2 methodIndex)
          val paramsCount = Descriptor.paramsCount descriptor
          val returnCount = Descriptor.returnCount descriptor
        in
          (bytes, returnCount - paramsCount, constPool)
        end
      | invokeinterface { class, name, descriptor } =>
        let
          val (methodIndex, constPool) =
            ConstPool.withMethodref constPool {
              class = class,
              nameAndType = {
                name = name,
                descriptor = descriptor
              }
            }
          val paramsCount = Descriptor.paramsCount descriptor
          val returnCount = Descriptor.returnCount descriptor
          val bytes = Word8Vector.prepend (0wxB9, u2 methodIndex)
          val bytes = Word8Vector.append (bytes, Word8.fromInt paramsCount)
          val bytes = Word8Vector.append (bytes, 0w0)
        in
          (bytes, returnCount - paramsCount - 1 (* this *), constPool)
        end
      | invokedynamic specifier =>
        let
          val (index, constPool) = ConstPool.withInvokeDynamic constPool specifier
          val bytes = Word8Vector.concat [vec [0wxBA], u2 index, u2 0]
        in
          (bytes, 0, constPool)
        end
      | new className =>
        let
          val (classIndex, constPool) = ConstPool.withClass constPool className
          val bytes = Word8Vector.prepend (0wxBB, u2 classIndex)
        in
          (bytes, 1, constPool)
        end
      | newarray arrayType => (vec [0wxBC, ArrayType.compile arrayType], 0, constPool)
      | anewarray index => (Word8Vector.prepend (0wxBD, u2 index), 0, constPool)
      | arraylength => (vec [0wxBE], 0, constPool)
      | athrow => (vec [0wxBF], 1, constPool)
      | checkcast index => (Word8Vector.prepend (0wxC0, u2 index), 0, constPool)
      | instanceof index => (Word8Vector.prepend (0wxC1, u2 index), 0, constPool)
      | monitorenter => (vec [0wxC2], 0, constPool)
      | monitorexit => (vec [0wxC3], 0, constPool)
      | goto offset => (Word8Vector.prepend (0wxA7, u2 offset), 0, constPool)
      | jsr offset => (Word8Vector.prepend (0wxA8, u2 offset), 1, constPool)
      | ret index => (vec [0wxA9, index], 0, constPool)
      | tableswitch => raise Fail "not implemented"
      | lookupswitch => raise Fail "not implemented"
      | ireturn => (vec [0wxAC], ~1, constPool)
      | lreturn => (vec [0wxAD], ~2, constPool)
      | freturn => (vec [0wxAE], ~1, constPool)
      | dreturn => (vec [0wxAF], ~2, constPool)
      | areturn => (vec [0wxB0], ~1, constPool)
      | return => (vec [0wxB1], 0, constPool)
      | wide => raise Fail "not implemented"
      | multianewarray (index, dimensions) =>
        let
          val bytes = Word8Vector.prepend (0wxC5, u2 index)
          val bytes = Word8Vector.append (bytes, dimensions)
        in
          (bytes, 1 - Word8.toInt dimensions, constPool)
        end
      | ifnull offset => (Word8Vector.prepend (0wxC6, u2 offset), ~1, constPool)
      | ifnonnull offset => (Word8Vector.prepend (0wxC7, u2 offset), ~1, constPool)
      | goto_w offset => (Word8Vector.prepend (0wxC8, u4 offset), 0, constPool)
      | jsr_w offset => (Word8Vector.prepend (0wxC9, u4 offset), 1, constPool)
      | breakpoint => (vec [0wxCA], 0, constPool)
      | impdep1 => (vec [0wxFE], 0, constPool)
      | impdep2 => (vec [0wxFF], 0, constPool)
  end
