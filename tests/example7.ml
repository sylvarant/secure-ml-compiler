struct

    module IsZero =
    struct
        val test x = x == 0
    end

    module PairTest =
      functor(Input: sig val test: int->bool end)
      struct
        val testfst p = (Input.test (fst p))
      end

    module PairTestZero = PairTest(IsZero)

    val testcase p = (PairTestZero.testfst p)
end
