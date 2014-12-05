(* generative functor test: seal the output*)
struct

    module IsZero =
    struct
        val test x = x == 0
    end

    module Test =
      functor(Input: sig val test: int->bool end)
      struct
        type t = bool
        val testfst p = (Input.test p)
      end : sig
        type t
        val testfst : int -> t
      end

    module TestZero = Test(IsZero)
    module TestZero2 = Test(IsZero)

end
