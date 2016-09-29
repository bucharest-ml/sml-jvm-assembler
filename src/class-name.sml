(**
 * https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.2.1
 *)
structure ClassName =
  struct
    type t = Text.t
    fun fromParts parts = Text.concatWith "/" parts
    fun fromString s = s
  end
