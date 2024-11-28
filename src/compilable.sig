(* Reader Monad *)
signature CONFIGURABLE =
  sig
    type 'a t
    type config

    val from : (config -> 'a) -> 'a t

    val get : key ->

    val run : config -> 'a t -> 'a
  end

structure Configurable =
  struct
    type 'computation t = int

    fun from f =
      raise Fail "not implemented"

    fun run config =
  end

Configurable.from (fn config =>

)

signature COMPILABLE =
  sig
    type t

    val compile : ConstPool.t -> t -> (Word8Vector.vector, ConstPool.t) Configurable.t
  end
