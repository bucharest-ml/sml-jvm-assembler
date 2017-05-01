structure Class =
  struct
    structure Flag =
      struct
        datatype t =
          PUBLIC
        | FINAL
        | SUPER
        | INTERFACE
        | ABSTRACT
        | SYNTHETIC
        | ANNOTATION
        | ENUM

        fun compile flag : Word.word =
          case flag of
            PUBLIC     => 0wx0001
          | FINAL      => 0wx0010
          | SUPER      => 0wx0020
          | INTERFACE  => 0wx0200
          | ABSTRACT   => 0wx0400
          | SYNTHETIC  => 0wx1000
          | ANNOTATION => 0wx2000
          | ENUM       => 0wx4000

        fun mask flags =
          let
            fun mask (flag, acc) = Word.orb (acc, compile flag)
          in
            List.foldl mask 0wx0 flags
          end
      end

    type t = {
      accessFlags : Flag.t list,
      thisClass : ClassName.t,
      superClass : ClassName.t,
      interfaces : ClassName.t list,
      fields : Field.t list,
      methods : Method.t list,
      attributes : Attr.t list
    }

    fun from (class : t) = class

    fun compile { accessFlags, thisClass, superClass, interfaces, fields, methods, attributes } =
      let
        open Util

        val constPool = ConstPool.empty
        val magic = vec [0wxCA, 0wxFE, 0wxBA, 0wxBE]
        val minorVersion = u2 0
        val majorVersion = u2 49
        val (thisClassIndex, constPool) = ConstPool.withClass constPool thisClass
        val (superClassIndex, constPool) = ConstPool.withClass constPool superClass

        fun addInterface (interface, (bytes, constPool)) =
          let
            val (ifaceIndex, constPool) = ConstPool.withClass constPool interface
          in
            (Word8Vector.concat [bytes, u2 ifaceIndex], constPool)
          end
        val (interfacesBytes, constPool) =
          List.foldl addInterface (vec [], constPool) interfaces

        fun compileMany compiler constPool list =
          let
            fun fold (attr, (bytes, constPool)) =
              let
                val (newBytes, constPool) = compiler constPool attr
              in
                (Word8Vector.concat [bytes, newBytes], constPool)
              end
          in
            List.foldl fold (vec [], constPool) list
          end

        val (fieldsBytes, constPool) = compileMany Field.compile constPool fields
        val (methodsBytes, constPool) = compileMany Method.compile constPool methods
        val bootstrapMethods = ConstPool.bootstrapMethods constPool
        val attributes =
          if List.null bootstrapMethods
          then attributes
          else Attr.BootstrapMethods bootstrapMethods :: attributes
        val (attrsBytes, constPool) = compileMany Attr.compile constPool attributes
        val constPoolBytes = ConstPool.compile constPool
      in
        Word8Vector.concat [
          magic,
          minorVersion,
          majorVersion,
          u2 (1 + ConstPool.length constPool),
          constPoolBytes,
          u2 (Word.toInt (Flag.mask accessFlags)),
          u2 thisClassIndex,
          u2 superClassIndex,
          u2 (List.length interfaces),
          interfacesBytes,
          u2 (List.length fields),
          fieldsBytes,
          u2 (List.length methods),
          methodsBytes,
          u2 (List.length attributes),
          attrsBytes
        ]
      end
  end
