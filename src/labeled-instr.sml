(*
 * Overlay instruction language over the standard JVM one that replaces offset
 * targets with labels. This eases code generation and hides offset management
 * inside the routine that compiles the instructions to actual bytes.
 *)
structure LabeledInstr =
  struct
    type label = string

    datatype t =
      INSTR of Instr.t
    | LABEL of label
    | GOTO of {
        label : label,
        instr : Instr.offset -> Instr.t,
        byteCount : int
      }

    val label           = LABEL
    val nop             = INSTR Instr.nop
    val aconst_null     = INSTR Instr.aconst_null
    val iconst_m1       = INSTR Instr.iconst_m1
    val iconst_0        = INSTR Instr.iconst_0
    val iconst_1        = INSTR Instr.iconst_1
    val iconst_2        = INSTR Instr.iconst_2
    val iconst_3        = INSTR Instr.iconst_3
    val iconst_4        = INSTR Instr.iconst_4
    val iconst_5        = INSTR Instr.iconst_5
    val lconst_0        = INSTR Instr.lconst_0
    val lconst_1        = INSTR Instr.lconst_1
    val fconst_0        = INSTR Instr.fconst_0
    val fconst_1        = INSTR Instr.fconst_1
    val fconst_2        = INSTR Instr.fconst_2
    val dconst_0        = INSTR Instr.dconst_0
    val dconst_1        = INSTR Instr.dconst_1
    val bipush          = INSTR o Instr.bipush
    val sipush          = INSTR o Instr.sipush
    val ldc             = INSTR o Instr.ldc
    val iload           = INSTR o Instr.iload
    val lload           = INSTR o Instr.lload
    val fload           = INSTR o Instr.fload
    val dload           = INSTR o Instr.dload
    val aload           = INSTR o Instr.aload
    val iload_0         = INSTR Instr.iload_0
    val iload_1         = INSTR Instr.iload_1
    val iload_2         = INSTR Instr.iload_2
    val iload_3         = INSTR Instr.iload_3
    val lload_0         = INSTR Instr.lload_0
    val lload_1         = INSTR Instr.lload_1
    val lload_2         = INSTR Instr.lload_2
    val lload_3         = INSTR Instr.lload_3
    val fload_0         = INSTR Instr.fload_0
    val fload_1         = INSTR Instr.fload_1
    val fload_2         = INSTR Instr.fload_2
    val fload_3         = INSTR Instr.fload_3
    val dload_0         = INSTR Instr.dload_0
    val dload_1         = INSTR Instr.dload_1
    val dload_2         = INSTR Instr.dload_2
    val dload_3         = INSTR Instr.dload_3
    val aload_0         = INSTR Instr.aload_0
    val aload_1         = INSTR Instr.aload_1
    val aload_2         = INSTR Instr.aload_2
    val aload_3         = INSTR Instr.aload_3
    val iaload          = INSTR Instr.iaload
    val laload          = INSTR Instr.laload
    val faload          = INSTR Instr.faload
    val daload          = INSTR Instr.daload
    val aaload          = INSTR Instr.aaload
    val baload          = INSTR Instr.baload
    val caload          = INSTR Instr.caload
    val saload          = INSTR Instr.saload
    val istore          = INSTR o Instr.istore
    val lstore          = INSTR o Instr.lstore
    val fstore          = INSTR o Instr.fstore
    val dstore          = INSTR o Instr.dstore
    val astore          = INSTR o Instr.astore
    val istore_0        = INSTR Instr.istore_0
    val istore_1        = INSTR Instr.istore_1
    val istore_2        = INSTR Instr.istore_2
    val istore_3        = INSTR Instr.istore_3
    val lstore_0        = INSTR Instr.lstore_0
    val lstore_1        = INSTR Instr.lstore_1
    val lstore_2        = INSTR Instr.lstore_2
    val lstore_3        = INSTR Instr.lstore_3
    val fstore_0        = INSTR Instr.fstore_0
    val fstore_1        = INSTR Instr.fstore_1
    val fstore_2        = INSTR Instr.fstore_2
    val fstore_3        = INSTR Instr.fstore_3
    val dstore_0        = INSTR Instr.dstore_0
    val dstore_1        = INSTR Instr.dstore_1
    val dstore_2        = INSTR Instr.dstore_2
    val dstore_3        = INSTR Instr.dstore_3
    val astore_0        = INSTR Instr.astore_0
    val astore_1        = INSTR Instr.astore_1
    val astore_2        = INSTR Instr.astore_2
    val astore_3        = INSTR Instr.astore_3
    val iastore         = INSTR Instr.iastore
    val lastore         = INSTR Instr.lastore
    val fastore         = INSTR Instr.fastore
    val dastore         = INSTR Instr.dastore
    val aastore         = INSTR Instr.aastore
    val bastore         = INSTR Instr.bastore
    val castore         = INSTR Instr.castore
    val sastore         = INSTR Instr.sastore
    val pop             = INSTR Instr.pop
    val pop2            = INSTR Instr.pop2
    val dup             = INSTR Instr.dup
    val dup_x1          = INSTR Instr.dup_x1
    val dup_x2          = INSTR Instr.dup_x2
    val dup2            = INSTR Instr.dup2
    val dup2_x1         = INSTR Instr.dup2_x1
    val dup2_x2         = INSTR Instr.dup2_x2
    val swap            = INSTR Instr.swap
    val iadd            = INSTR Instr.iadd
    val ladd            = INSTR Instr.ladd
    val fadd            = INSTR Instr.fadd
    val dadd            = INSTR Instr.dadd
    val isub            = INSTR Instr.isub
    val lsub            = INSTR Instr.lsub
    val fsub            = INSTR Instr.fsub
    val dsub            = INSTR Instr.dsub
    val imul            = INSTR Instr.imul
    val lmul            = INSTR Instr.lmul
    val fmul            = INSTR Instr.fmul
    val dmul            = INSTR Instr.dmul
    val idiv            = INSTR Instr.idiv
    val ldiv            = INSTR Instr.ldiv
    val fdiv            = INSTR Instr.fdiv
    val ddiv            = INSTR Instr.ddiv
    val irem            = INSTR Instr.irem
    val lrem            = INSTR Instr.lrem
    val frem            = INSTR Instr.frem
    val drem            = INSTR Instr.drem
    val ineg            = INSTR Instr.ineg
    val lneg            = INSTR Instr.lneg
    val fneg            = INSTR Instr.fneg
    val dneg            = INSTR Instr.dneg
    val ishl            = INSTR Instr.ishl
    val lshl            = INSTR Instr.lshl
    val ishr            = INSTR Instr.ishr
    val lshr            = INSTR Instr.lshr
    val iushr           = INSTR Instr.iushr
    val lushr           = INSTR Instr.lushr
    val iand            = INSTR Instr.iand
    val land            = INSTR Instr.land
    val ior             = INSTR Instr.ior
    val lor             = INSTR Instr.lor
    val ixor            = INSTR Instr.ixor
    val lxor            = INSTR Instr.lxor
    val iinc            = INSTR o Instr.iinc
    val i2l             = INSTR Instr.i2l
    val i2f             = INSTR Instr.i2f
    val i2d             = INSTR Instr.i2d
    val l2i             = INSTR Instr.l2i
    val l2f             = INSTR Instr.l2f
    val l2d             = INSTR Instr.l2d
    val f2i             = INSTR Instr.f2i
    val f2l             = INSTR Instr.f2l
    val f2d             = INSTR Instr.f2d
    val d2i             = INSTR Instr.d2i
    val d2l             = INSTR Instr.d2l
    val d2f             = INSTR Instr.d2f
    val i2b             = INSTR Instr.i2b
    val i2c             = INSTR Instr.i2c
    val i2s             = INSTR Instr.i2s
    val lcmp            = INSTR Instr.lcmp
    val fcmpl           = INSTR Instr.fcmpl
    val fcmpg           = INSTR Instr.fcmpg
    val dcmpl           = INSTR Instr.dcmpl
    val dcmpg           = INSTR Instr.dcmpg
    fun ifeq label      = GOTO { label = label, instr = Instr.ifeq, byteCount = 3 }
    fun ifne label      = GOTO { label = label, instr = Instr.ifne, byteCount = 3 }
    fun iflt label      = GOTO { label = label, instr = Instr.iflt, byteCount = 3 }
    fun ifge label      = GOTO { label = label, instr = Instr.ifge, byteCount = 3 }
    fun ifgt label      = GOTO { label = label, instr = Instr.ifgt, byteCount = 3 }
    fun ifle label      = GOTO { label = label, instr = Instr.ifle, byteCount = 3 }
    fun if_icmpeq label = GOTO { label = label, instr = Instr.if_icmpeq, byteCount = 3 }
    fun if_icmpne label = GOTO { label = label, instr = Instr.if_icmpne, byteCount = 3 }
    fun if_icmplt label = GOTO { label = label, instr = Instr.if_icmplt, byteCount = 3 }
    fun if_icmpge label = GOTO { label = label, instr = Instr.if_icmpge, byteCount = 3 }
    fun if_icmpgt label = GOTO { label = label, instr = Instr.if_icmpgt, byteCount = 3 }
    fun if_icmple label = GOTO { label = label, instr = Instr.if_icmple, byteCount = 3 }
    fun if_acmpeq label = GOTO { label = label, instr = Instr.if_acmpeq, byteCount = 3 }
    fun if_acmpne label = GOTO { label = label, instr = Instr.if_acmpne, byteCount = 3 }
    val getstatic       = INSTR o Instr.getstatic
    val putstatic       = INSTR o Instr.putstatic
    val getfield        = INSTR o Instr.getfield
    val putfield        = INSTR o Instr.putfield
    val invokevirtual   = INSTR o Instr.invokevirtual
    val invokespecial   = INSTR o Instr.invokespecial
    val invokestatic    = INSTR o Instr.invokestatic
    val invokeinterface = INSTR o Instr.invokeinterface
    val invokedynamic   = INSTR o Instr.invokedynamic
    val new             = INSTR o Instr.new
    val newarray        = INSTR o Instr.newarray
    val anewarray       = INSTR o Instr.anewarray
    val arraylength     = INSTR Instr.arraylength
    val athrow          = INSTR Instr.athrow
    val checkcast       = INSTR o Instr.checkcast
    val instanceof      = INSTR o Instr.instanceof
    val monitorenter    = INSTR Instr.monitorenter
    val monitorexit     = INSTR Instr.monitorexit
    fun goto label      = GOTO { label = label, instr = Instr.goto, byteCount = 3 }
    fun jsr label       = GOTO { label = label, instr = Instr.jsr, byteCount = 3 }
    val ret             = INSTR o Instr.ret
    val tableswitch     = INSTR Instr.tableswitch
    val lookupswitch    = INSTR Instr.lookupswitch
    val ireturn         = INSTR Instr.ireturn
    val lreturn         = INSTR Instr.lreturn
    val freturn         = INSTR Instr.freturn
    val dreturn         = INSTR Instr.dreturn
    val areturn         = INSTR Instr.areturn
    val return          = INSTR Instr.return
    val wide            = INSTR Instr.wide
    val multianewarray  = INSTR o Instr.multianewarray
    fun ifnull label    = GOTO { label = label, instr = Instr.ifnull, byteCount = 3 }
    fun ifnonnull label = GOTO { label = label, instr = Instr.ifnonnull, byteCount = 3 }
    fun goto_w label    = GOTO { label = label, instr = Instr.goto_w, byteCount = 5 }
    fun jsr_w label     = GOTO { label = label, instr = Instr.jsr_w, byteCount = 5 }
    val breakpoint      = INSTR Instr.breakpoint
    val impdep1         = INSTR Instr.impdep1
    val impdep2         = INSTR Instr.impdep2

    fun toString instr =
      case instr of
        INSTR instr => "INSTR " ^ Instr.toString instr
      | LABEL label => "LABEL " ^ label
      | GOTO { label, instr, ... } =>
          "GOTO (" ^ label ^ ", " ^ Instr.toString (instr 0) ^ ")"

    structure LabelMap = BinaryMapFn(struct
      type ord_key = string
      val compare = String.compare
    end)

    fun compileList constPool instrs =
      let
        fun traverseLabel label state rest =
          let
            val { index, offset, constPool, stackSize, maxStack, ... } = state
            val { maxLocals, bytes, seenLabels, offsetedInstrs, ... } = state
          in
            traverse rest {
              index = index,
              offset = offset,
              constPool = constPool,
              stackSize = stackSize,
              maxStack = maxStack,
              maxLocals = maxLocals,
              bytes = bytes,
              seenLabels = LabelMap.insert (seenLabels, label, (offset, index)),
              offsetedInstrs = offsetedInstrs
            }
          end

        and traverseInstr instr state rest =
          let
            val { index, offset, constPool, stackSize, maxStack, ... } = state
            val { maxLocals, bytes, seenLabels, offsetedInstrs, ... } = state
            val (opcodes, stackDiff, constPool) = Instr.compile constPool instr
            val storeIndex = Option.getOpt (Instr.storeIndex instr, 0) + 1
          in
            traverse rest {
              index = index + 1,
              offset = offset + Word8Vector.length opcodes,
              constPool = constPool,
              stackSize = stackSize + stackDiff,
              maxStack = Int.max (maxStack, stackSize + stackDiff),
              maxLocals = Int.max (maxLocals, storeIndex),
              bytes = Word8Vector.concat [bytes, opcodes],
              seenLabels = seenLabels,
              offsetedInstrs = (offset, instr) :: offsetedInstrs
            }
          end

        and traverseGoto { label, instr, byteCount } state rest =
          let
            val { index, offset, constPool, stackSize, maxStack, ... } = state
            val { maxLocals, bytes, seenLabels, offsetedInstrs, ... } = state
          in
            case LabelMap.find (seenLabels, label) of
              SOME (labelOffset, labelIndex) =>
              let
                val offsetedInstr = instr labelIndex
                val instr = instr (labelOffset - offset)
                val (opcodes, stackDiff, constPool) = Instr.compile constPool instr
              in
                traverse rest {
                  index = index + 1,
                  offset = offset + Word8Vector.length opcodes,
                  constPool = constPool,
                  stackSize = stackSize + stackDiff,
                  maxStack = Int.max (maxStack, stackSize + stackDiff),
                  maxLocals = maxLocals,
                  bytes = Word8Vector.concat [bytes, opcodes],
                  seenLabels = seenLabels,
                  offsetedInstrs = (offset, offsetedInstr) :: offsetedInstrs
                }
              end
            | NONE =>
              let
                (*
                 * We don't have a label yet; traverse the rest of the
                 * instruction stream and then try again, maybe a label
                 * has been found.
                 *)
                val result = traverse rest {
                  index = index + 1,
                  offset = offset + byteCount,
                  constPool = constPool,
                  (*
                   * We don't have an instruction stackDiff here, so we just
                   * reset these counters and compensate later.
                   *)
                  stackSize = 0,
                  maxStack = 0,
                  maxLocals = maxLocals,
                  bytes = Util.vec [],
                  seenLabels = seenLabels,
                  offsetedInstrs = []
                }
              in
                case LabelMap.find (#seenLabels result, label) of
                  NONE => raise Fail ("undefined label: " ^ label)
                | SOME (labelOffset, labelIndex) =>
                  let
                    (*
                     * We're doing a kind of a dirty thing here. We're misusing
                     * the instruction's offset field by putting the *index* of
                     * the target instruction. The index as it appears in our
                     * instruction list, not in the final byte stream.
                     *)
                    val offsetedInstr = instr labelIndex
                    val instr = instr (labelOffset - offset)
                    val (opcodes, stackDiff, constPool) =
                      Instr.compile (#constPool result) instr
                  in
                    {
                      (*
                       * These values are only read inside recursive calls, not
                       * when the function returns, so nobody will look at them,
                       * which means we can use default values and save some
                       * computations.
                       *)
                      index = 0,
                      offset = 0,
                      stackSize = 0,
                      seenLabels = LabelMap.empty,

                      (*
                       * The following are values that will be read on return,
                       * so we have to put the real values.
                       *)
                      constPool = constPool,
                      (*
                       * Here's where we compensate for the fact that above we
                       * didn't know the stack diff amount of an instruction.
                       *)
                      maxStack = Int.max (
                        maxStack,
                        #maxStack result + stackSize + stackDiff
                      ),
                      maxLocals = #maxLocals result,
                      bytes = Word8Vector.concat [
                        bytes,
                        opcodes,
                        #bytes result
                      ],
                      offsetedInstrs = List.concat [
                        #offsetedInstrs result,
                        [(offset, offsetedInstr)],
                        offsetedInstrs
                      ]
                    }
                  end
              end
          end

        and traverse instrs state =
          case instrs of
            [] => state
          | (LABEL label :: rest) => traverseLabel label state rest
          | (INSTR instr :: rest) => traverseInstr instr state rest
          | (GOTO goto :: rest) => traverseGoto goto state rest

        val seed = {
          index = 0,
          offset = 0,
          constPool = constPool,
          stackSize = 0,
          maxStack = 0,
          maxLocals = 0,
          bytes = Util.vec [],
          seenLabels = LabelMap.empty,
          offsetedInstrs = []
        }

        val { bytes, maxStack, maxLocals, constPool, offsetedInstrs, ... } =
          traverse instrs seed
      in
        {
          bytes = bytes,
          maxStack = maxStack,
          maxLocals = maxLocals,
          constPool = constPool,
          offsetedInstrs = List.rev offsetedInstrs
        }
      end
  end
