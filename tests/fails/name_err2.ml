(* ERRORCODE 3 *)
struct

    module PairTest =
      functor(input: sig val test: int->bool end)
      struct
        val testfst p = (input.test (fst p))
      end

end
