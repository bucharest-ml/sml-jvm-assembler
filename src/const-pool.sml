structure ConstPool :> CONST_POOL =
  struct
    type entry_index = int
    type reference_kind = int
    type bootstrap_method_attr_index = int
    type name_and_type = { name : Text.t, descriptor : Text.t }
    type symbol_ref = { class : Text.t, nameAndType : name_and_type }
    type call_site = {
      nameAndType : name_and_type,
      bootstrapMethod : {
        methodHandle : MethodHandle.t,
        symbolRef : symbol_ref,
        methodParams : unit list (* TODO *)
      }
    }

    type bootstrap_method = {
      methodRef : entry_index,
      arguments : entry_index list
    }

    structure BootstrapMethodsMap = RedBlackMapFn(struct
      type ord_key = bootstrap_method
      fun compare (a : bootstrap_method, b : bootstrap_method) =
        case Int.compare (#methodRef a, #methodRef b) of
          EQUAL => List.collate Int.compare (#arguments a, #arguments b)
        | other => other
    end)

    datatype entry =
      Class              of entry_index
    | String             of entry_index
    | Utf8               of Text.t
    | Long               of Long.t
    | Float              of Float.t
    | Double             of Double.t
    | Integer            of Integer.t
    | MethodType         of entry_index
    | NameAndType        of entry_index * entry_index
    | Fieldref           of entry_index * entry_index
    | Methodref          of entry_index * entry_index
    | InterfaceMethodref of entry_index * entry_index
    | InvokeDynamic      of bootstrap_method_attr_index * entry_index
    | MethodHandle       of reference_kind * entry_index

    structure Map = RedBlackMapFn(struct
      type ord_key = entry

      fun ordinal entry =
        case entry of
          Class _ => 0
        | String _ => 1
        | Utf8 _ => 2
        | Long _ => 3
        | Float _ => 4
        | Double _ => 5
        | Integer _ => 6
        | MethodType _ => 7
        | NameAndType _ => 8
        | Fieldref _ => 9
        | Methodref _ => 10
        | InterfaceMethodref _ => 11
        | InvokeDynamic _ => 12
        | MethodHandle _ => 13

      fun tupleCompare ((a, x), (b, y)) =
        case Int.compare (a, b) of
          EQUAL => Int.compare (x, y)
        | other => other

      fun compare operands =
        case operands of
          (Class a, Class b) => Int.compare (a, b)
        | (String a, String b) => Int.compare (a, b)
        | (Utf8 a, Utf8 b) => Text.compare (a, b)
        | (Long a, Long b) => Long.compare (a, b)
        | (Float a, Float b) => Float.compare (a, b)
        | (Double a, Double b) => Double.compare (a, b)
        | (Integer a, Integer b) => Integer.compare (a, b)
        | (MethodType a, MethodType b) => Int.compare (a, b)
        | (NameAndType a, NameAndType b) => tupleCompare (a, b)
        | (Fieldref a, Fieldref b) => tupleCompare (a, b)
        | (Methodref a, Methodref b) => tupleCompare (a, b)
        | (InterfaceMethodref a, InterfaceMethodref b) => tupleCompare (a, b)
        | (InvokeDynamic a, InvokeDynamic b) => tupleCompare (a, b)
        | (MethodHandle a, MethodHandle b) => tupleCompare (a, b)
        | (a, b) => Int.compare (ordinal a, ordinal b)
    end)

    type t = {
      counter : int,
      entries : int Map.map,
      bootstrap : {
        counter : int,
        entries : int BootstrapMethodsMap.map
      }
    }

    val empty = {
      counter = 0,
      entries = Map.empty,
      bootstrap = {
        counter = 0,
        entries = BootstrapMethodsMap.empty
      }
    }

    fun length { counter, entries, bootstrap } = counter

    fun withEntry (constPool as { counter, entries, bootstrap }) entry =
      case Map.find (entries, entry) of
        SOME entryIndex => (entryIndex, constPool)
      | NONE =>
          let
            val counter = counter + 1
            val constPool = {
              counter = counter,
              entries = Map.insert (entries, entry, counter),
              bootstrap = bootstrap
            }
          in
            (counter, constPool)
          end

    fun withUtf8 constPool text = withEntry constPool (Utf8 text)
    fun withInteger constPool value = withEntry constPool (Integer value)
    fun withLong constPool value = withEntry constPool (Long value)
    fun withFloat constPool value = withEntry constPool (Float value)
    fun withDouble constPool value = withEntry constPool (Double value)

    fun withClass constPool name =
      let
        val (index, constPool) = withUtf8 constPool name
      in
        withEntry constPool (Class index)
      end

    fun withString constPool text =
      let
        val (index, constPool) = withUtf8 constPool text
      in
        withEntry constPool (String index)
      end

    fun withMethodType constPool text =
      let
        val (index, constPool) = withUtf8 constPool text
      in
        withEntry constPool (MethodType index)
      end

    fun withNameAndType constPool { name, descriptor } =
      let
        val (nameIndex, constPool) = withUtf8 constPool name
        val (descIndex, constPool) = withUtf8 constPool descriptor
      in
        withEntry constPool (NameAndType (nameIndex, descIndex))
      end

    fun withSymbolRef kind constPool { class, nameAndType } =
      let
        val (classIndex, constPool) = withClass constPool class
        val (nameTypeIndex, constPool) = withNameAndType constPool nameAndType
      in
        withEntry constPool (kind (classIndex, nameTypeIndex))
      end

    fun withFieldref constPool = withSymbolRef Fieldref constPool
    fun withMethodref constPool = withSymbolRef Methodref constPool
    fun withInterfaceMethodref constPool = withSymbolRef InterfaceMethodref constPool

    fun withMethodHandle constPool { kind, symbolRef } =
      let
        open MethodHandle
        val makeSymbolRef =
          case kind of
            GetField => withSymbolRef Fieldref
          | GetStatic => withSymbolRef Fieldref
          | PutField => withSymbolRef Fieldref
          | PutStatic => withSymbolRef Fieldref
          | InvokeVirtual => withSymbolRef Methodref
          | InvokeStatic => withSymbolRef Methodref (* TODO: Add support for InterfaceMethodref *)
          | InvokeSpecial => withSymbolRef Methodref (* TODO: Add support for InterfaceMethodref *)
          | NewInvokeSpecial => withSymbolRef Methodref
          | InvokeInterface => withSymbolRef InterfaceMethodref
        val methodKind = MethodHandle.value kind
        val (refIndex, constPool) = makeSymbolRef constPool symbolRef
      in
        withEntry constPool (MethodHandle (methodKind, refIndex))
      end

    fun withInvokeDynamic constPool { nameAndType, bootstrapMethod } =
      let
        val { methodHandle, symbolRef, methodParams } = bootstrapMethod

        val (methodHandleIndex, constPool) = withMethodHandle constPool {
          kind = methodHandle,
          symbolRef = symbolRef
        }

        val (bootstrapMethodIndex, constPool) =
          let
            val { counter, entries, bootstrap = { counter = bootCounter, entries = bootEntries } } = constPool
            val entry = { methodRef = methodHandleIndex, arguments = [] }
          in
            case BootstrapMethodsMap.find (bootEntries, entry) of
              SOME entryIndex => (entryIndex, constPool)
            | NONE =>
              let
                val newEntries = BootstrapMethodsMap.insert (bootEntries, entry, bootCounter)
              in
                (bootCounter, {
                  counter = counter,
                  entries = entries,
                  bootstrap = {
                    counter = bootCounter + 1,
                    entries = newEntries
                  }
                })
              end
          end

        val (nameTypeIndex, constPool) = withNameAndType constPool nameAndType
      in
        withEntry constPool (InvokeDynamic (bootstrapMethodIndex, nameTypeIndex))
      end

    fun bootstrapMethods { counter, entries, bootstrap } =
      let
        fun greater ((_, i), (_, j)) = Int.compare (i, j) = GREATER
        val { counter, entries } = bootstrap
      in
        List.map #1 (ListMergeSort.sort greater (BootstrapMethodsMap.listItemsi entries))
      end

    fun compile { counter, entries, bootstrap } =
      let
        open Util

        fun compareItems ((_, i), (_, j)) = Int.compare (i, j) = GREATER

        val sortedEntries =
          List.map #1 (ListMergeSort.sort compareItems (Map.listItemsi entries))

        fun compileEntry (entry, bytes) =
          let
            val entryBytes =
              case entry of
                Class entryIndex => Word8Vector.concat [vec [0wx7], u2 entryIndex]
              | String entryIndex => Word8Vector.concat [vec [0wx8], u2 entryIndex]
              | Utf8 value =>
                let
                  val bytes = Text.compile value
                in
                  Word8Vector.concat [
                    vec [0w1],
                    u2 (Word8Vector.length bytes),
                    bytes
                  ]
                end
              | Long value => raise Fail "not implemented"
              | Float value => raise Fail "not implemented"
              | Double value => raise Fail "not implemented"
              | Integer value => raise Fail "not implemented"
              | MethodType entryIndex => raise Fail "not implemented"
              | NameAndType (entryIndex1, entryIndex2) =>
                  Word8Vector.concat [
                    vec [0w12],
                    u2 entryIndex1,
                    u2 entryIndex2
                  ]
              | Fieldref (entryIndex1, entryIndex2) =>
                  Word8Vector.concat [
                    vec [0w9],
                    u2 entryIndex1,
                    u2 entryIndex2
                  ]
              | Methodref (entryIndex1, entryIndex2) =>
                  Word8Vector.concat [
                    vec [0w10],
                    u2 entryIndex1,
                    u2 entryIndex2
                  ]
              | InterfaceMethodref (entryIndex1, entryIndex2) => raise Fail "not implemented"
              | InvokeDynamic (bootstrapMethodAttrIndex, entryIndex) =>
                  Word8Vector.concat [
                    vec [0w18],
                    u2 bootstrapMethodAttrIndex,
                    u2 entryIndex
                  ]
              | MethodHandle (referenceKind, entryIndex) =>
                  Word8Vector.concat [
                    vec [0w15],
                    vec [Word8.fromInt referenceKind],
                    u2 entryIndex
                  ]
          in
            Word8Vector.concat [bytes, entryBytes]
          end
        val seed = vec []
      in
        List.foldl compileEntry seed sortedEntries
      end
  end
