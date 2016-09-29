signature CONST_POOL =
  sig
    type t

    (* Aliases for readability. *)
    type entry_index = int
    type name_and_type = { name : Text.t, descriptor : Text.t }
    type symbol_ref = { class : Text.t, nameAndType : name_and_type }

    val empty : t
    val length : t -> int

    val withUtf8 : t -> Text.t -> entry_index * t
    val withInteger : t -> Integer.t -> entry_index * t
    val withLong : t -> Long.t -> entry_index * t
    val withFloat : t -> Float.t -> entry_index * t
    val withDouble : t -> Double.t -> entry_index * t
    val withClass : t -> Text.t -> entry_index * t
    val withString : t -> Text.t -> entry_index * t
    val withMethodType : t -> Text.t -> entry_index * t
    val withNameAndType : t -> name_and_type -> entry_index * t
    val withFieldref : t -> symbol_ref -> entry_index * t
    val withMethodref : t -> symbol_ref -> entry_index * t
    val withInterfaceMethodref : t -> symbol_ref -> entry_index * t
    val withInvokeDynamic : t -> { bootstrapMethodIndex : int, nameAndType : name_and_type } -> entry_index * t
    val withMethodHandle : t -> { kind : MethodHandle.t, symbolRef : symbol_ref } -> entry_index * t

    val compile : t -> Word8Vector.vector
  end