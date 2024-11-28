signature VERIFIER =
  sig
    val verify : ('offset * Instr.t) list -> { offset : 'offset, instrs : StackLang.t list } list
  end
