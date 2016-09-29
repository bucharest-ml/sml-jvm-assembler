structure MethodHandle =
  struct
    datatype t =
      GetField
    | GetStatic
    | PutField
    | PutStatic
    | InvokeVirtual
    | InvokeStatic
    | InvokeSpecial
    | NewInvokeSpecial
    | InvokeInterface

    fun value kind =
      case kind of
        GetField => 1
      | GetStatic => 2
      | PutField => 3
      | PutStatic => 4
      | InvokeVirtual => 5
      | InvokeStatic => 6
      | InvokeSpecial => 7
      | NewInvokeSpecial => 8
      | InvokeInterface => 9
  end
