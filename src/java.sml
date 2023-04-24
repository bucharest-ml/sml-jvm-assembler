structure java =
  let
    structure D = Descriptor
    val class = ClassName.fromString
  in
    struct
      structure lang =
        struct
          structure Integer =
            struct
              val toString = {
                class = class "java/lang/Integer",
                name = "toString",
                descriptor = D.Method {
                  params = [D.Int],
                  return = D.Type (D.Object (class "java/lang/String"))
                }
              }
            end

          structure System =
            struct
              val out = {
                class = class "java/lang/System",
                name = "out",
                descriptor = D.Field (D.Object (class "java/io/PrintStream"))
              }
            end
        end

      structure io =
        struct
          structure PrintStream =
            struct
              val println = {
                class = class "java/io/PrintStream",
                name = "println",
                descriptor = D.Method {
                  params = [D.Object (class "java/lang/String")],
                  return = D.Void
                }
              }
            end
        end
    end
  end
