structure StackLang =
  struct
    type local_index = int

    datatype t =
    | Push of VerificationType.t
    | Pop of VerificationType.t
    | Load of local_index * VerificationType.t
    | Store of local_index * VerificationType.t
    | Local of local_index * VerificationType.t
    | Branch of { targetOffset : int, fallsThrough : bool }

    exception StackUnderflow
    exception UnassignedLocal

    fun toString t =
      case t of
      | Push vtype => "Push " ^ VerificationType.toString vtype
      | Pop vtype => "Pop " ^ VerificationType.toString vtype
      | Load (index, vtype) => "Load ("^ Int.toString index ^", "^ VerificationType.toString vtype ^")"
      | Store (index, vtype) => "Store ("^ Int.toString index ^", "^ VerificationType.toString vtype ^")"
      | Local (index, vtype) => "Local ("^ Int.toString index ^", "^ VerificationType.toString vtype ^")"
      | Branch { targetOffset, fallsThrough } =>
          "Branch { targetOffset = "^ Int.toString targetOffset ^", fallsThrough = "^ Bool.toString fallsThrough ^" }"

    fun interpret instrs =
      let
        fun mergeFrames prev curr =
          let
            val { stack = prevStack, locals = prevLocals } = prev
            val { stack = currStack, locals = currLocals } = curr
          in
            if List.length prevStack <> List.length currStack
            then raise Fail "mergeFrames: different stack lengths"
            else
              let
                val mergedLocals =
                  ListPair.map (Fn.uncurry VerificationType.leastUpperBound) (prevLocals, currLocals)
              in
                {
                  stack = currStack,
                  locals = mergedLocals
                }
              end
          end

        fun generateFrame instrs state =
          let
            (* TODO: handle longs and doubles which occupy two slots *)
            fun fold (instr, { stack, locals, frameMap, fallsThrough }) =
              case instr of
              | Push vType => {
                  stack = vType :: stack,
                  locals = locals,
                  frameMap = frameMap,
                  fallsThrough = true
                }
              | Pop vType => {
                  stack = List.tl stack handle Empty => raise StackUnderflow,
                  locals = locals,
                  frameMap = frameMap,
                  fallsThrough = true
                }
              | Load (index, vType) => {
                  stack = vType :: stack,
                  locals = locals,
                  frameMap = frameMap,
                  fallsThrough = true
                }
              | Local (index, vType) => {
                  stack = stack,
                  locals = List.update (locals, index, vType) handle Subscript => raise UnassignedLocal,
                  frameMap = frameMap,
                  fallsThrough = true
                }
              | Store (index, vType) => {
                  stack = List.tl stack handle Empty => raise StackUnderflow,
                  locals = List.update (locals, index, vType) handle Subscript => raise UnassignedLocal,
                  frameMap = frameMap,
                  fallsThrough = true
                }
              | Branch { targetOffset, fallsThrough } =>
                let
                  val frame = { stack = stack, locals = locals }
                  val mergedFrame =
                    case IntBinaryMap.find (frameMap, targetOffset) of
                      NONE => { offset = NONE, frame = frame, isBranchTarget = true }
                    | SOME { offset, frame = prevFrame, isBranchTarget } => {
                        offset = offset,
                        frame = mergeFrames prevFrame frame,
                        isBranchTarget = true
                      }
                in
                  {
                    stack = stack,
                    locals = locals,
                    frameMap = IntBinaryMap.insert (frameMap, targetOffset, mergedFrame),
                    fallsThrough = fallsThrough
                  }
                end
          in
            List.foldl fold state instrs
          end

        fun eval instrss =
          let
            fun fold (index, { offset, instrs }, { stack, locals, frameMap, fallsThrough }) =
              let
                val frame = { stack = stack, locals = locals }
                val mergedFrame =
                  case IntBinaryMap.find (frameMap, index) of
                  | NONE => {
                      offset = SOME offset,
                      frame = frame,
                      isBranchTarget = false
                    }
                  | SOME { offset = NONE, frame = prevFrame, isBranchTarget } => {
                      offset = SOME offset,
                      frame = mergeFrames prevFrame frame,
                      isBranchTarget = isBranchTarget
                    }
                  | SOME { offset, frame = prevFrame, isBranchTarget } => {
                      offset = offset,
                      frame = mergeFrames prevFrame frame,
                      isBranchTarget = isBranchTarget
                    }
                val frameMap = IntBinaryMap.insert (frameMap, index, mergedFrame)
              in
                generateFrame instrs {
                  stack = if fallsThrough then stack else [],
                  locals = locals,
                  frameMap = frameMap,
                  fallsThrough = true
                }
              end

            val seed = {
              stack = [],
              locals = [VerificationType.Reference, VerificationType.Top, VerificationType.Top], (* TODO *)
              frameMap = IntBinaryMap.empty,
              fallsThrough = true
            }
          in
            List.foldli fold seed instrss
          end

        fun unwrapOffset (index, item) =
          case item of
          | { offset = NONE, ... } => raise Fail ("bug: NONE offset at index: " ^ Int.toString index)
          | { offset = SOME offset, frame, isBranchTarget } => {
              offset = offset,
              frame = frame,
              isBranchTarget = isBranchTarget
            }
      in
        List.mapi unwrapOffset (IntBinaryMap.listItems (#frameMap (eval instrs)))
      end

    (* TODO: update function *)
    fun compile frameSets =
      let
        fun compile ({ offset, frames }, { prevLocals, compiled, lastOffset }) =
          let
            val isBranchTarget = List.length frames > 1
            val offsetDelta = offset - lastOffset
          in
            case List.hd frames of
            | { stack = [], locals } =>
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
      | [] => []
      | { frame = { locals, stack }, offset, isBranchTarget } :: frameSets =>
        let
          fun compile ({ offset, frame, isBranchTarget }, state as { prevLocals, compiled, prevOffset }) =
            let
              (* val isBranchTarget = List.length frames > 1 *)
              val offsetDelta = offset - (if prevOffset = 0 then prevOffset else prevOffset + 1)
            in
              if not isBranchTarget
              then state
              else
                (* TODO: intersect frames *)
                case frame of
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
                      | Frame.Append n =>
                        let
                          val locals =
                            case List.drop (locals, List.length locals - n) of
                              [a, VerificationType.Top, VerificationType.Top] => [a]
                            | [a, b, VerificationType.Top] => [a, b]
                            | [a, VerificationType.Top] => [a]
                            | other => other
                        in
                          StackMap.Append {
                            offsetDelta = offsetDelta,
                            extraLocals = List.length locals,
                            locals = locals
                          }
                        end
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
