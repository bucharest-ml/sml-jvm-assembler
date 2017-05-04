(* §4.7.4 *)
structure StackMap =
  struct
    datatype frame =
      (*
       * This frame type indicates that the frame has exactly the same local
       * variables as the previous frame and that the operand stack is empty.
       *)
      Same of { offsetDelta : int }

      (*
       * This frame type indicates that the frame has exactly the same local
       * variables as the previous frame and that the operand stack has one
       * entry.
       *)
    | SameLocals1StackItem of {
        offsetDelta : int,
        stack: VerificationType.t
      }

      (*
       * This frame type indicates that the frame has the same local variables
       * as the previous frame except that the last k local variables are
       * absent, and that the operand stack is empty.
       *)
    | Chop of {
        offsetDelta : int,
        minusLocals : int
      }

      (*
       * This frame type indicates that the frame has the same locals as the
       * previous frame except that k additional locals are defined, and that
       * the operand stack is empty.
       *)
    | Append of {
        offsetDelta : int,
        extraLocals : int,
        locals : VerificationType.t list
      }

      (*
       * This frame contains all the stack and locals information, explicitly.
       *)
    | Full of {
        offsetDelta : int,
        locals : VerificationType.t list,
        stack : VerificationType.t list
      }

    open Util

    fun compile constPool frame =
      case frame of
        Same { offsetDelta } =>
          if offsetDelta <= 63
          then (u1 offsetDelta, constPool)
          else (Word8Vector.prepend (0w251, u2 offsetDelta), constPool)
      | SameLocals1StackItem { offsetDelta, stack } =>
        let
          val (vtype, constPool) = VerificationType.compile constPool stack
        in
          if offsetDelta <= 63
          then (Word8Vector.concat [u1 (offsetDelta + 64), vtype], constPool)
          else (Word8Vector.concat [u1 247, u2 offsetDelta, vtype], constPool)
        end
      | Chop { minusLocals, offsetDelta } =>
          if minusLocals < 1 orelse minusLocals > 3
          then raise Fail ("chop frame with invalid minusLocals value: " ^ Int.toString minusLocals)
          else (Word8Vector.concat [u1 (251 - minusLocals), u2 offsetDelta], constPool)
      | Append { extraLocals, offsetDelta, locals } =>
          if extraLocals < 1 orelse extraLocals > 3
          then raise Fail ("append frame with invalid extraLocals value: " ^ Int.toString extraLocals)
          else
            let
              val (localBytes, constPool) = VerificationType.compileList constPool locals
              val bytes = Word8Vector.concat [
                u1 (251 + extraLocals),
                u2 offsetDelta,
                localBytes
              ]
            in
              (bytes, constPool)
            end
      | Full { offsetDelta, locals, stack } =>
        let
          val (localBytes, constPool) = VerificationType.compileList constPool locals
          val (stackBytes, constPool) = VerificationType.compileList constPool stack
          val bytes = Word8Vector.concat [
            u1 255,
            u2 offsetDelta,
            u2 (List.length locals),
            localBytes,
            u2 (List.length stack),
            stackBytes
          ]
        in
          (bytes, constPool)
        end

    fun compileFrames constPool frames =
      List.foldMapState frames {
        monoid = Word8Vector.join,
        step = Fn.swap compile,
        seed = (vec [], constPool)
      }
  end
