(* a more complicated functor *)
struct

    module IsZero =
    struct
        val test x : int = x == 0
        val id x : bool = x
    end

    module Test =
      functor(Input: sig val test: int->bool end)
      struct
        val testfst p : int = (Input.test p)
        (*module Original = Input*)
        module New = struct
            val five = 5 
        end
        module Origin = Input (* TODO *)
      end 

    module TestZero = Test(IsZero)

end
