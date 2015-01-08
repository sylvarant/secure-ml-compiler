(* generative functor test: seal the output*)
struct

    module IsZero =
    struct
        val test x : int = x == 0
        val id x : bool = x
    end

    module Test =
      functor(Input: sig val test: int->bool end)
      struct
        type t = bool
        val testfst p : int = (Input.test p)
        val input x : t = x
      end : sig
        type t
        val testfst : int -> t
        val input : t -> bool
      end

    module TestZero = Test(IsZero)
    module TestZero2 = Test(IsZero)

end
