(*
 * Overlay instruction language over the standard JVM one that replaces offset
 * targets with labels. This eases code generation and hides offset management
 * inside the routine that compiles the instructions to actual bytes.
 *)
structure LabeledInstr =
  struct
    type label = string

    datatype t =
      GOTO of (Instr.offset -> Instr.t) * label
    | LABEL of label
    | INSTR of Instr.t

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
    fun ifeq label      = GOTO (Instr.ifeq, label)
    fun ifne label      = GOTO (Instr.ifne, label)
    fun iflt label      = GOTO (Instr.iflt, label)
    fun ifge label      = GOTO (Instr.ifge, label)
    fun ifgt label      = GOTO (Instr.ifgt, label)
    fun ifle label      = GOTO (Instr.ifle, label)
    fun if_icmpeq label = GOTO (Instr.if_icmpeq, label)
    fun if_icmpne label = GOTO (Instr.if_icmpne, label)
    fun if_icmplt label = GOTO (Instr.if_icmplt, label)
    fun if_icmpge label = GOTO (Instr.if_icmpge, label)
    fun if_icmpgt label = GOTO (Instr.if_icmpgt, label)
    fun if_icmple label = GOTO (Instr.if_icmple, label)
    fun if_acmpeq label = GOTO (Instr.if_acmpeq, label)
    fun if_acmpne label = GOTO (Instr.if_acmpne, label)
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
    fun goto label      = GOTO (Instr.goto, label)
    fun jsr label       = GOTO (Instr.jsr, label)
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
    fun ifnull label    = GOTO (Instr.ifnull, label)
    fun ifnonnull label = GOTO (Instr.ifnonnull, label)
    fun goto_w label    = GOTO (Instr.goto_w, label)
    fun jsr_w label     = GOTO (Instr.jsr_w, label)
    val breakpoint      = INSTR Instr.breakpoint
    val impdep1         = INSTR Instr.impdep1
    val impdep2         = INSTR Instr.impdep2

    fun compileList constPool instrs =
      let
        fun loop (instr, { constPool, stackSize, maxStack, maxLocals, bytes }) =
          case instr of
            GOTO label => raise Fail "not implemented"
          | LABEL label => raise Fail "not implemented"
          | INSTR instr =>
            let
              val (opcodes, stackDiff, constPool) = Instr.compile constPool instr
            in
              {
                constPool = constPool,
                stackSize = stackSize + stackDiff,
                maxStack = Int.max (maxStack, stackSize + stackDiff),
                maxLocals = maxLocals,
                bytes = Word8Vector.concat [bytes, opcodes]
              }
            end

        val seed = {
          constPool = constPool,
          stackSize = 0,
          maxStack = 0,
          maxLocals = 10, (* TODO: compute maxLocals *)
          bytes = Util.vec []
        }

        val result = List.foldl loop seed instrs
      in
        {
          bytes = #bytes result,
          maxStack = #maxStack result,
          maxLocals = #maxLocals result,
          constPool = #constPool result
        }
      end
  end
