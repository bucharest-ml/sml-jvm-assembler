structure StackLang =
  struct
    type local_index = int

    datatype t =
      Push of VerificationType.t
    | Pop of VerificationType.t
    | Load of local_index * VerificationType.t
    | Store of local_index * VerificationType.t
    | Local of local_index * VerificationType.t
    | Branch of { targetOffset : int }

    exception StackUnderflow

    fun interpret instrs =
      let
        fun generateFrame instrs state =
          let
            (* TODO: handle longs and doubles which occupy two slots *)
            fun fold (instr, { stack, locals, frameMap }) =
              case instr of
                Push vType => {
                  stack = vType :: stack,
                  locals = locals,
                  frameMap = frameMap
                }
              | Pop vType => {
                  stack = List.tl stack handle Empty => raise StackUnderflow,
                  locals = locals,
                  frameMap = frameMap
                }
              | Load (index, vType) => {
                  stack = vType :: stack,
                  locals = locals,
                  frameMap = frameMap
                }
              | Local (index, vType) => {
                  stack = stack,
                  locals = List.update (locals, index, vType),
                  frameMap = frameMap
                }
              | Store (index, vType) => {
                  stack = List.tl stack handle Empty => raise StackUnderflow,
                  locals = List.update (locals, index, vType),
                  frameMap = frameMap
                }
              | Branch { targetOffset } =>
                let
                  val frame = { stack = stack, locals = locals }
                  val frames =
                    case IntBinaryMap.find (frameMap, targetOffset) of
                      NONE => { offset = NONE, frames = [frame] }
                    | SOME { offset, frames } => { offset = offset, frames = frame :: frames }
                in
                  {
                    stack = stack,
                    locals = locals,
                    frameMap = IntBinaryMap.insert (frameMap, targetOffset, frames)
                  }
                end

          in
            List.foldl fold state instrs
          end

        fun eval instrss =
          let
            fun fold (index, { offset, instrs }, { stack, locals, frameMap }) =
              let
                val frame = { stack = stack, locals = locals }
                val frames =
                  case IntBinaryMap.find (frameMap, index) of
                    NONE => { offset = SOME offset, frames = [frame] }
                  | SOME { offset = NONE, frames } => { offset = SOME offset, frames = frame :: frames }
                  | SOME { offset, frames } => { offset = offset, frames = frame :: frames }
                val frameMap = IntBinaryMap.insert (frameMap, index, frames)
              in
                generateFrame instrs {
                  stack = stack,
                  locals = locals,
                  frameMap = frameMap
                }
              end

            val seed = {
              stack = [],
              locals = [VerificationType.Reference, VerificationType.Top, VerificationType.Top], (* TODO *)
              frameMap = IntBinaryMap.empty
            }
          in
            List.foldli fold seed instrss
          end

        fun unwrapOffset item =
          case item of
            { offset = NONE, frames } => raise Fail "bug: NONE offset"
          | { offset = SOME offset, frames } => { offset = offset, frames = frames }
      in
        List.map unwrapOffset (IntBinaryMap.listItems (#frameMap (eval instrs)))
      end

    fun compile frameSets =
      let
        fun compile ({ offset, frames }, { prevLocals, compiled, lastOffset }) =
          let
            val isBranchTarget = List.length frames > 1
            val offsetDelta = offset - lastOffset
          in
            (* TODO: intersect frames *)
            case List.hd frames of
              { stack = [], locals } =>
              let
                val localsSize = List.length locals
                val lastLocalsSize = List.length prevLocals
                val localsDiff = lastLocalsSize - localsSize
                val stackMapFrame =
                  if localsDiff = 0
                  then StackMap.Same { offsetDelta = offsetDelta }
                  else
                    if localsDiff < 0
                    then StackMap.Chop {
                      offsetDelta = offsetDelta,
                      minusLocals = localsDiff
                    }
                    else StackMap.Append {
                      offsetDelta = offsetDelta,
                      extraLocals = localsDiff,
                      locals = List.drop (locals, lastLocalsSize)
                    }
              in
                {
                  prevLocals = locals,
                  compiled = (isBranchTarget, stackMapFrame) :: compiled,
                  lastOffset = offset
                }
              end
            | { stack = [a], locals } =>
              let
                val localsSize = List.length locals
                val lastLocalsSize = List.length prevLocals
                val localsDiff = lastLocalsSize - localsSize
                val stackMapFrame =
                  if localsDiff = 0
                  then StackMap.SameLocals1StackItem { offsetDelta = offsetDelta, stack = a }
                  else StackMap.Full {
                    offsetDelta = offsetDelta,
                    stack = [a],
                    locals = locals
                  }
              in
                {
                  prevLocals = locals,
                  compiled = (isBranchTarget, stackMapFrame) :: compiled,
                  lastOffset = offset
                }
              end
            | { stack, locals } =>
              let
                val stackMapFrame = StackMap.Full {
                  offsetDelta = offsetDelta,
                  stack = stack,
                  locals = locals
                }
              in
                {
                  prevLocals = locals,
                  compiled = (isBranchTarget, stackMapFrame) :: compiled,
                  lastOffset = offset
                }
              end
          end

        val state = {
          prevLocals = [],
          compiled = [],
          lastOffset = 0
        }
      in
        List.rev (#compiled (List.foldl compile state frameSets))
      end

    fun compileCompact frameSets =
      case frameSets of
        [] => []
      | { frames = [{ locals, stack }], offset } :: frameSets =>
        let
          fun compile ({ offset, frames }, state as { prevLocals, compiled, prevOffset }) =
            let
              val isBranchTarget = List.length frames > 1
              val offsetDelta = offset - (if prevOffset = 0 then prevOffset else prevOffset + 1)
            in
              if not isBranchTarget
              then state
              else
                (* TODO: intersect frames *)
                case List.hd frames of
                  { stack = [], locals } =>
                  let
                    val stackMapFrame =
                      case Frame.localsDifference prevLocals locals of
                        Frame.Same => StackMap.Same { offsetDelta = offsetDelta }
                      | Frame.Full => StackMap.Full {
                          offsetDelta = offsetDelta,
                          stack = [],
                          locals = locals
                        }
                      | Frame.Chop n => StackMap.Chop {
                          offsetDelta = offsetDelta,
                          minusLocals = n
                        }
                      | Frame.Append n => StackMap.Append {
                          offsetDelta = offsetDelta,
                          extraLocals = n,
                          locals = List.drop (locals, List.length locals - n)
                        }
                  in
                    {
                      prevLocals = locals,
                      compiled = stackMapFrame :: compiled,
                      prevOffset = offset
                    }
                  end
                | { stack = [a], locals } =>
                  let
                    val localsSize = List.length locals
                    val lastLocalsSize = List.length prevLocals
                    val localsDiff = lastLocalsSize - localsSize
                    val stackMapFrame =
                      if localsDiff = 0
                      then StackMap.SameLocals1StackItem { offsetDelta = offsetDelta, stack = a }
                      else StackMap.Full {
                        offsetDelta = offsetDelta,
                        stack = [a],
                        locals = locals
                      }
                  in
                    {
                      prevLocals = locals,
                      compiled = stackMapFrame :: compiled,
                      prevOffset = offset
                    }
                  end
                | { stack, locals } =>
                  let
                    val stackMapFrame = StackMap.Full {
                      offsetDelta = offsetDelta,
                      stack = stack,
                      locals = locals
                    }
                  in
                    {
                      prevLocals = locals,
                      compiled = stackMapFrame :: compiled,
                      prevOffset = offset
                    }
                  end
            end

          val state = {
            prevOffset = 0,
            prevLocals = locals,
            compiled = []
          }
        in
          List.rev (#compiled (List.foldl compile state frameSets))
        end
  end
