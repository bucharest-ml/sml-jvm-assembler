structure Main =
  struct
    fun symbol class name descriptor = {
      class = ClassName.fromString class,
      name = name,
      descriptor = Descriptor.fromString descriptor
    }

    val class = Class.from {
      accessFlags = [Class.Flag.PUBLIC],
      thisClass = ClassName.fromString "Main",
      superClass = ClassName.fromString "java/lang/Object",
      interfaces = [],
      attributes = [],
      fields = [
        Field.from {
          accessFlags = [Field.Flag.PRIVATE, Field.Flag.STATIC, Field.Flag.FINAL],
          name = "message",
          descriptor = Descriptor.Field (Descriptor.Object (ClassName.fromString "java/lang/String")),
          attributes = [
            Attr.ConstantValue (ConstantValue.String "Hello, World!")
          ]
        }
      ],
      methods = [
        Method.from {
          accessFlags = [Method.Flag.PUBLIC, Method.Flag.STATIC],
          name = "main",
          (* descriptor = Descriptor.fromString "([Ljava/lang/String;)V", *)
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
                Instr.getstatic (symbol "java/lang/System" "out" "Ljava/io/PrintStream;"),
                Instr.getstatic (symbol "Main" "message" "Ljava/lang/String;"),
                (* Instr.ldc_string "Hello, World!", *)
                (* Instr.iconst_1,
                Instr.iconst_5,
                Instr.iadd,
                Instr.iconst_3,
                Instr.idiv, *)
                (* Instr.invokevirtual (symbol "java/io/PrintStream" "println" "(I)V"), *)
                Instr.invokevirtual (symbol "java/io/PrintStream" "println" "(Ljava/lang/String;)V"),
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
      ]
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
  end
