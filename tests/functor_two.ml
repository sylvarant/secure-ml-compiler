(* simple functor example for stamp testing*)
struct

    module Input =
    struct
        val test x : int = x == 0
    end

    module Test1 =
      functor(Input: sig val test: int->bool end)
      struct
        val testfst p : int = (Input.test p)
      end

    module Test2 =
      functor(Input: sig val test: int->bool end)
      struct
        val testfst p : int = (Input.test p)
      end
end
