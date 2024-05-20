signature INDEXED_INSTR =
  sig
    type t

    val index : t -> Index.t
    val instr : t -> Instr.t
  end


structure IndexedInstr : INDEXED_INSTR =
  struct
    type t = (Index.t, Instr.t)

    val fromPair = Fn.id

    fun index (i, _) => i
    fun instr (_, i) => i
  end
