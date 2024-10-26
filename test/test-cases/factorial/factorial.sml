structure Factorial =
  let
    val className = "Factorial"

    structure D = Descriptor
    structure Instr = LabeledInstr

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
          code = let open Instr in [
            iconst_5,
            invokestatic {
              class = ClassName.fromString className,
              name = "factorial",
              descriptor = D.Method {
                params = [D.Int],
                return = D.Type D.Int
              }
            },
            istore_1,
            getstatic java.lang.System.out,
            iload_1,
            invokestatic java.lang.Integer.toString,
            invokevirtual java.io.PrintStream.println,
            return
          ] end
        }
      ]
    }

    val factorial = Method.from {
      name = "factorial",
      accessFlags = [Method.Flag.PUBLIC, Method.Flag.STATIC],
      descriptor = Descriptor.Method {
        return = Descriptor.Type Descriptor.Int,
        params = [
          Descriptor.Int
        ]
      },
      attributes = [
        Attr.Code {
          exceptionTable = [],
          attributes = [],
          (*
             0: Push Integer
             1: Store (1, Integer)
             2: Load (0, Integer)
             3: Pop Integer; Branch { targetOffset = 10, fallsThrough = true }
             6: Load (0, Integer)
             7: Load (1, Integer)
             8: Pop Integer; Pop Integer; Push Integer
             9: Store (1, Integer)
            10: Local (0, Integer)
            13: Branch { targetOffset = 2, fallsThrough = false }
            16: Load (1, Integer)
            17: Pop Integer
          *)
          code = let open Instr in [
            iconst_1,            (* int r = 1; *)
            istore_1,
            label "enter-while", (* while (n > 0) { *)
            iload_0,
            ifle "exit-while",
            iload_0,             (* r = n * r; *)
            iload_1,
            imul,
            istore_1,
            iinc (0w0, ~ 0w1),   (* n-- *)
            goto "enter-while",  (* } *)
            label "exit-while",
            iload_1,             (* return r; *)
            ireturn
          ] end
        }
      ]
    }
  in
    (* DSL sketch attempt *)
    (* let open Assembly.DSL in
      class [PUBLIC] "Factorial" [] [] [
        field [PRIVATE, STATIC, FINAL] "java/lang/String" "Hello, World!"

        method [PUBLIC, STATIC] INT "factorial" [INT] [
          iconst_1,
          istore_1,
          label "enter-while",
          iload_0,
          ifle "exit-while",
          iload_0,
          iload_1,
          imul,
          istore_1,
          iinc (0w0, ~ 0w1),
          goto "enter-while",
          label "exit-while",
          iload_1,
          ireturn
        ] end
      ]
    end *)

    struct
      fun class name = Class.from {
        accessFlags = [Class.Flag.PUBLIC],
        thisClass = ClassName.fromString name,
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
        methods = [main, factorial]
      }

      val trim =
        let open Char Substring in
          string o dropl isSpace o dropr isSpace o full
        end

      fun java { classpath } className =
        let
          val proc = Unix.execute ("/usr/bin/java", ["-cp", classpath, className])
          val output = TextIO.inputAll (Unix.textInstreamOf proc)
        in
          Unix.reap proc
        ; trim output
        end

      fun main () =
        let
          val workDir = OS.FileSys.getDir ()
          val binDir = OS.Path.joinDirFile { dir = workDir, file = "bin" }
          val fileName = OS.Path.joinDirFile { dir = binDir, file = className ^ ".class" }
          val classFile = BinIO.openOut fileName
          val bytes = Class.compile (class className)
          val _ = BinIO.output (classFile, bytes)
          val _ = BinIO.closeOut classFile
          val output = java { classpath = binDir } className
        in
          print (output ^ "\n")
        end
    end
  end
