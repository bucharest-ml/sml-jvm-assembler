structure FrameSuite : TEST_SUITE =
  struct
    infix -->
    infix ===

    open TestSuite

    val all = [
      "1 < 2" --> (fn _ =>
        Ints.assert 1 op< 2
      ),

      "1 = 1" --> let in fn _ =>
        1 === 2
        (* Ints.assert 1 op= 2 *)
      end
    ]
  end
