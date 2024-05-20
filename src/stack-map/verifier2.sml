(*
 * Write it using the final tagless approach? Is it worth it?
 *)
(* structure StackLang =
  struct
    type t = int
  end *)



structure Verifier2 :
  sig
    val verify : Instr.t list -> StackMap.frame list
  end
  =
  struct
    fun verify instrs =
      let
        fun fold (instr, state) =
          raise Fail "not implemented"

        val seed = []

        val r = List.foldl fold seed instrs
      in
        raise Fail "not implemented"
      end
  end
