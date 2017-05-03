structure Main =
  struct
    structure Instr = LabeledInstr

    fun symbol class name descriptor = {
      class = ClassName.fromString class,
      name = name,
      descriptor = Descriptor.fromString descriptor
    }

    val bootstrap = Method.from {
      name = "bootstrap",
      accessFlags = [Method.Flag.PRIVATE, Method.Flag.STATIC],
      descriptor = Descriptor.Method {
        return = Descriptor.Type (Descriptor.Object (ClassName.fromString "java/lang/invoke/CallSite")),
        params = [
          Descriptor.Object (ClassName.fromString "java/lang/invoke/MethodHandles$Lookup"),
          Descriptor.Object (ClassName.fromString "java/lang/String"),
          Descriptor.Object (ClassName.fromString "java/lang/invoke/MethodType"),
          Descriptor.Object (ClassName.fromString "java/lang/Class")
        ]
      },
      attributes = [
        Attr.Code {
          exceptionTable = [],
          attributes = [],
          code = [
            Instr.new (ClassName.fromString "java/lang/invoke/ConstantCallSite"),
            Instr.dup,
            Instr.aload_0,
            Instr.aload_3,
            Instr.aload_1,
            Instr.aload_2,
            Instr.invokevirtual {
              class = ClassName.fromString "java/lang/invoke/MethodHandles$Lookup",
              name = "findStatic",
              descriptor = Descriptor.Method {
                return = Descriptor.Type (Descriptor.Object (ClassName.fromString "java/lang/invoke/MethodHandle")),
                params = [
                  Descriptor.Object (ClassName.fromString "java/lang/Class"),
                  Descriptor.Object (ClassName.fromString "java/lang/String"),
                  Descriptor.Object (ClassName.fromString "java/lang/invoke/MethodType")
                ]
              }
            },
            Instr.invokespecial {
              class = ClassName.fromString "java/lang/invoke/ConstantCallSite",
              name = "<init>",
              descriptor = Descriptor.Method {
                return = Descriptor.Void,
                params = [
                  Descriptor.Object (ClassName.fromString "java/lang/invoke/MethodHandle")
                ]
              }
            },
            Instr.areturn
          ]
        }
      ]
    }

    val printString = Method.from {
      name = "print",
      accessFlags = [Method.Flag.PRIVATE, Method.Flag.STATIC],
      descriptor = Descriptor.Method {
        return = Descriptor.Void,
        params = [
          Descriptor.Object (ClassName.fromString "java/lang/String")
        ]
      },
      attributes = [
        Attr.Code {
          exceptionTable = [],
          attributes = [],
          code = [
            Instr.getstatic (symbol "java/lang/System" "out" "Ljava/io/PrintStream;"),
            Instr.aload_0,
            Instr.invokevirtual (symbol "java/io/PrintStream" "println" "(Ljava/lang/String;)V"),
            Instr.return
          ]
        }
      ]
    }

    val main = Method.from {
      name = "main",
      accessFlags = [Method.Flag.PUBLIC, Method.Flag.STATIC],
      descriptor = Descriptor.Method {
        return = Descriptor.Void,
        params = [
          Descriptor.Array (Descriptor.Object (ClassName.fromString "java/lang/String"))
        ]
      },
      attributes = [
        Attr.Code {
          exceptionTable = [],
          attributes = [],
          code = [
            Instr.ldc (Const.String "Hello, InvokeDynamic!"),
            Instr.invokedynamic {
              nameAndType = {
                name = "print",
                descriptor = Descriptor.Method {
                  return = Descriptor.Void,
                  params = [
                    Descriptor.Object (ClassName.fromString "java/lang/String")
                  ]
                }
              },
              bootstrapMethod = {
                methodHandle = MethodHandle.InvokeStatic,
                symbolRef = {
                  class = ClassName.fromString "Main",
                  nameAndType = Method.nameAndType bootstrap
                },
                methodParams = [Const.Class "Main"]
              }
            },
            (* Instr.getstatic (symbol "java/lang/System" "out" "Ljava/io/PrintStream;"), *)
            (* Instr.getstatic (symbol "Main" "message" "Ljava/lang/String;"), *)
            (* Instr.ldc (Const.String "Hello, World!"), *)
            (* Instr.iconst_1,
            Instr.iconst_5,
            Instr.iadd,
            Instr.iconst_3,
            Instr.idiv, *)
            (* Instr.invokevirtual (symbol "java/io/PrintStream" "println" "(I)V"), *)
            (* Instr.invokevirtual (symbol "java/io/PrintStream" "println" "(Ljava/lang/String;)V"), *)
            Instr.return
          ]
        },
        Attr.Exceptions [
          ClassName.fromString "java/lang/Exception",
          ClassName.fromString "java/io/IOException"
        ],
        Attr.Synthetic,
        Attr.Deprecated
      ]
    }

    val withBranch = Method.from {
      name = "main",
      accessFlags = [Method.Flag.PUBLIC, Method.Flag.STATIC],
      descriptor = Descriptor.Method {
        return = Descriptor.Void,
        params = [
          Descriptor.Array (Descriptor.Object (ClassName.fromString "java/lang/String"))
        ]
      },
      attributes = [
        Attr.Code {
          exceptionTable = [],
          attributes = [],
          code = let open Instr in [
            aload_0,
            arraylength,
            iconst_0,
            if_icmpne "else",
            getstatic java.lang.System.out,
            ldc (Const.String "T"),
            invokevirtual java.io.PrintStream.println,
            goto "return",
            label "else",
            getstatic java.lang.System.out,
            ldc (Const.String "F"),
            invokevirtual java.io.PrintStream.println,
            label "return",
            return
          ] end
        }
      ]
    }

    val factorial = Method.from {
      name = "main",
      accessFlags = [Method.Flag.PUBLIC, Method.Flag.STATIC],
      descriptor = Descriptor.Method {
        return = Descriptor.Void,
        params = [
          Descriptor.Array (Descriptor.Object (ClassName.fromString "java/lang/String"))
        ]
      },
      attributes = [
        Attr.Code {
          exceptionTable = [],
          attributes = [],
          code = let open Instr in [
            iconst_5,
            istore_1,
            iconst_1,
            istore_2,
            label "enter-while",
            iload_1,
            ifle "exit-while",
            iload_2,
            iload_1,
            imul,
            istore_2,
            iinc (0w1, Word8.fromInt ~1),
            goto "enter-while",
            label "exit-while",
            getstatic java.lang.System.out,
            iload_2,
            invokestatic java.lang.Integer.toString,
            invokevirtual java.io.PrintStream.println,
            return
          ] end
        }
      ]
    }

    val class = Class.from {
      accessFlags = [Class.Flag.PUBLIC],
      thisClass = ClassName.fromString "Main",
      superClass = ClassName.fromString "java/lang/Object",
      interfaces = [],
      attributes = [Attr.SourceFile "main.sml"],
      fields = [
        Field.from {
          name = "message",
          accessFlags = [Field.Flag.PRIVATE, Field.Flag.STATIC, Field.Flag.FINAL],
          descriptor = Descriptor.Field (Descriptor.Object (ClassName.fromString "java/lang/String")),
          attributes = [
            Attr.ConstantValue (ConstantValue.String "Hello, World!")
          ]
        }
      ],
      (* methods = [main, printString, bootstrap] *)
      methods = [factorial]
    }

    val trim =
      let open Char Substring in
        string o dropl isSpace o dropr isSpace o full
      end

    fun java classPath className =
      let
        val proc = Unix.execute ("/usr/bin/java", ["-cp", classPath, className])
        val output = TextIO.inputAll (Unix.textInstreamOf proc)
      in
        Unix.reap proc
      ; trim output
      end

    fun main () =
      let
        val workDir = OS.FileSys.getDir ()
        val bytes = Class.compile class
        val f = BinIO.openOut (OS.Path.joinDirFile { dir = workDir, file = "Main.class" })
        val _ = BinIO.output (f, bytes)
        val _ = BinIO.closeOut f
        val output = java workDir "Main"
      in
        print (output ^ "\n")
      end

    fun stackMap () =
      let
        val { attributes = [Attr.Code { code, ... }], ... } = factorial
        val { offsetedInstrs, ... } = Instr.compileList ConstPool.empty code
      in
        StackLang.compileCompact
        (
          StackLang.interpret
          (
            Verifier.verify offsetedInstrs
          )
        )
      end
  end
