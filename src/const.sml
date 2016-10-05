structure Const =
  struct
    datatype t =
      Integer of Integer.t
    | Float of Float.t
    | Long of Long.t
    | Double of Double.t
    | String of Text.t
    | Class of ClassName.t
    | MethodType of Text.t
    | MethodHandle of {
        kind : MethodHandle.t,
        symbolRef : {
          class : Text.t,
          nameAndType : {
            name : Text.t,
            descriptor : Descriptor.t
          }
        }
      }
  end
