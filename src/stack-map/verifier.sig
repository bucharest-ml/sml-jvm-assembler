signature VERIFIER =
  sig
    val verify : Instr.t list -> StackLang.t list list
  end
