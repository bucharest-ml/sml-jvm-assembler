structure ExceptionInfo =
  struct
    type t = string
  end

structure ConstantValue =
  struct
    datatype t =
      Integer of Integer.t
    | Long of Long.t
    | Float of Float.t
    | Double of Double.t
    | String of Text.t
  end

(* https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7 *)
structure Attr =
  struct
    open Util

    datatype t =
      Custom
    | ConstantValue of ConstantValue.t
    | Code of {
        code : Instr.t list,
        exceptionTable : ExceptionInfo.t list,
        attributes : t list
      }
    | StackMapTable
    | Exceptions of ClassName.t list
    | BootstrapMethods of ConstPool.bootstrap_method list
    | InnerClasses
    | EnclosingMethod
    | Synthetic
    | Signature of Text.t
    | RuntimeVisibleAnnotations
    | RuntimeInvisibleAnnotations
    | RuntimeVisibleParameterAnnotations
    | RuntimeInvisibleParameterAnnotations
    | RuntimeVisibleTypeAnnotations
    | RuntimeInvisibleTypeAnnotations
    | AnnotationDefault
    | MethodParameters
    | SourceFile of Text.t
    | SourceDebugExtension
    | LineNumberTable
    | LocalVariableTable
    | LocalVariableTypeTable
    | Deprecated

    fun compile constPool attr =
      case attr of
        Code code => compileCode constPool code
      | ConstantValue value => compileConstantValue constPool value
      | Exceptions exceptions => compileExceptions constPool exceptions
      | Synthetic => compileSynthetic constPool
      | Deprecated => compileDeprecated constPool
      | Signature typeSignature => compileSignature constPool typeSignature
      | SourceFile value => compileSourceFile constPool value
      | BootstrapMethods methods => compileBootstrapMethods constPool methods
      | attribute => raise Fail "not implemented"

    (* https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.3 *)
    and compileCode constPool { code, exceptionTable, attributes } =
      let
        fun compileExceptions constPool exceptionTable =
          (u2 0, constPool) (* TODO: add exceptions *)

        fun compileAttributes constPool attributes =
          (u2 0, constPool) (* TODO: add attributes *)

        val (attrNameIndex, constPool) = ConstPool.withUtf8 constPool "Code"
        val (instrBytes, constPool) = compileInstructions constPool code
        val (exceptionBytes, constPool) = compileExceptions constPool exceptionTable
        val (attributeBytes, constPool) = compileAttributes constPool attributes
        val attributeLength =
          Word8Vector.length instrBytes +
          Word8Vector.length exceptionBytes +
          Word8Vector.length attributeBytes
        val bytes = Word8Vector.concat [
          u2 attrNameIndex,
          u4 attributeLength,
          instrBytes,
          exceptionBytes,
          attributeBytes
        ]
      in
        (bytes, constPool)
      end

    and compileInstructions constPool code =
      let
        fun compile (instr, { stackSize, maxStack, maxLocals, constPool, bytes }) =
          let
            val (opcodes, stackDiff, constPool) = Instr.compile constPool instr
          in
            {
              stackSize = stackSize + stackDiff,
              maxStack = Int.max (maxStack, stackSize + stackDiff),
              maxLocals = maxLocals,
              constPool = constPool,
              bytes = Word8Vector.concat [bytes, opcodes]
            }
          end

        val seed = {
          stackSize = 0,
          maxStack = 0,
          maxLocals = 10, (* TODO: compute maxLocals *)
          constPool = constPool,
          bytes = vec []
        }

        val { maxStack, maxLocals, constPool, bytes, ... } =
          List.foldl compile seed code

        val bytes = Word8Vector.concat [
          u2 maxStack,
          u2 maxLocals,
          u4 (Word8Vector.length bytes),
          bytes
        ]
      in
        (bytes, constPool)
      end

    and compileConstantValue constPool value =
      let
        val (attrNameIndex, constPool) = ConstPool.withUtf8 constPool "ConstantValue"
        val (constValueIndex, constPool) =
          case value of
            ConstantValue.Integer value => raise Fail "not implemented"
          | ConstantValue.Long value => raise Fail "not implemented"
          | ConstantValue.Float value => raise Fail "not implemented"
          | ConstantValue.Double value => raise Fail "not implemented"
          | ConstantValue.String value => ConstPool.withString constPool value
        val attributeLength = 2
        val bytes = Word8Vector.concat [
          u2 attrNameIndex,
          u4 attributeLength,
          u2 constValueIndex
        ]
      in
        (bytes, constPool)
      end

    and compileExceptions constPool exceptions =
      let
        fun fold (ex, (bytes, constPool)) =
          let
            val (exIndex, constPool) = ConstPool.withClass constPool ex
          in
            (Word8Vector.concat [bytes, u2 exIndex], constPool)
          end
        val seed = (vec [], constPool)
        val (exceptionBytes, constPool) = List.foldl fold seed exceptions
        val (attrIndex, constPool) = ConstPool.withUtf8 constPool "Exceptions"
        val attributeLength = 2 + Word8Vector.length exceptionBytes
        val bytes = Word8Vector.concat [
          u2 attrIndex,
          u4 attributeLength,
          u2 (List.length exceptions),
          exceptionBytes
        ]
      in
        (bytes, constPool)
      end

    and compileSynthetic constPool =
      let
        val attributeLength = 0
        val (attrIndex, constPool) = ConstPool.withUtf8 constPool "Synthetic"
        val bytes = Word8Vector.concat [u2 attrIndex, u4 attributeLength]
      in
        (bytes, constPool)
      end

    and compileDeprecated constPool =
      let
        val attributeLength = 0
        val (attrIndex, constPool) = ConstPool.withUtf8 constPool "Deprecated"
        val bytes = Word8Vector.concat [u2 attrIndex, u4 attributeLength]
      in
        (bytes, constPool)
      end

    and compileSignature constPool typeSignature =
      let
        val attributeLength = 2
        val (attrIndex, constPool) = ConstPool.withUtf8 constPool "Signature"
        val (sigIndex, constPool) = ConstPool.withUtf8 constPool typeSignature
        val bytes = Word8Vector.concat [
          u2 attrIndex,
          u4 attributeLength,
          u2 sigIndex
        ]
      in
        (bytes, constPool)
      end

    and compileSourceFile constPool value =
      let
        val attributeLength = 2
        val (attrIndex, constPool) = ConstPool.withUtf8 constPool "SourceFile"
        val (valueIndex, constPool) = ConstPool.withUtf8 constPool value
        val bytes = Word8Vector.concat [
          u2 attrIndex,
          u4 attributeLength,
          u2 valueIndex
        ]
      in
        (bytes, constPool)
      end

    and compileBootstrapMethods constPool methods =
      let
        fun addMethod ({ methodRef, arguments }, bytes) =
          let
            val argBytes = Word8Vector.concat (List.map u2 arguments)
          in
            Word8Vector.concat [
              bytes,
              u2 methodRef,
              u2 (List.length arguments),
              argBytes
            ]
          end
        val (attrIndex, constPool) = ConstPool.withUtf8 constPool "BootstrapMethods"
        val methodBytes = List.foldl addMethod (vec []) methods
        val attributeLength = 2 + Word8Vector.length methodBytes
        val bytes = Word8Vector.concat [
          u2 attrIndex,
          u4 attributeLength,
          u2 (List.length methods),
          methodBytes
        ]
      in
        (bytes, constPool)
      end

    fun minimumVersion attr =
      case attr of
        Custom                               => { major = 45, minor = 3 }
      | SourceFile _                         => { major = 45, minor = 3 }
      | InnerClasses                         => { major = 45, minor = 3 }
      | ConstantValue _                      => { major = 45, minor = 3 }
      | Code _                               => { major = 45, minor = 3 }
      | Exceptions _                         => { major = 45, minor = 3 }
      | Synthetic                            => { major = 45, minor = 3 }
      | LineNumberTable                      => { major = 45, minor = 3 }
      | LocalVariableTable                   => { major = 45, minor = 3 }
      | Deprecated                           => { major = 45, minor = 3 }
      | EnclosingMethod                      => { major = 49, minor = 0 }
      | SourceDebugExtension                 => { major = 49, minor = 0 }
      | RuntimeVisibleParameterAnnotations   => { major = 49, minor = 0 }
      | RuntimeInvisibleParameterAnnotations => { major = 49, minor = 0 }
      | AnnotationDefault                    => { major = 49, minor = 0 }
      | Signature _                          => { major = 49, minor = 0 }
      | RuntimeVisibleAnnotations            => { major = 49, minor = 0 }
      | RuntimeInvisibleAnnotations          => { major = 49, minor = 0 }
      | LocalVariableTypeTable               => { major = 49, minor = 0 }
      | StackMapTable                        => { major = 50, minor = 0 }
      | BootstrapMethods _                   => { major = 51, minor = 0 }
      | MethodParameters                     => { major = 52, minor = 0 }
      | RuntimeVisibleTypeAnnotations        => { major = 52, minor = 0 }
      | RuntimeInvisibleTypeAnnotations      => { major = 52, minor = 0 }
  end
