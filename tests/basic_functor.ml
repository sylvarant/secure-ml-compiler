struct

    module IsZero =
    struct
        val test x : int = x == 0
    end

    module PairTest =
      functor(Input: sig val test: int->bool end)
      struct
        val testfst p : int * int = (Input.test (fst p))
      end

    module PairTestZero = PairTest(IsZero)

    val testcase p = (PairTestZero.testfst p)
end
