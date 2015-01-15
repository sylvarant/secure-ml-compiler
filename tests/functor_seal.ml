(* generative functor test: seal the output*)
struct

    (* For use by attacker/ static tests*)
    module IsZero =
    struct
        val test x : int = x == 0
        val id x : bool = x
    end

    (* for use in arg test *)
    module Argument =
    struct
        type t = int
        val test x : t = x + 10
        val id x : t = x
    end

    (* basic sealing test *)
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

    module TestStatic = Test(IsZero)

    (* Seal on inner functor as wel as outer context *)
    module InnerTest =
      functor(Input: sig val test: int->bool end)
      struct
        type t = bool
        val id x : t = x
        module Inner =
        struct
            val testfst p : int = (Input.test p)
            val input x : t = x
        end
      end : sig
        type t
        val id : t -> t
        module Inner: sig
            val testfst : int -> t
            val input : t -> bool
        end
      end

    module TestInnerStatic = InnerTest(IsZero)

    (* Test the sealing of objects created by returned functor *)
    module FunctorTest =
      functor(Ignored1: sig val test: int->bool end)
        functor(Ignored2 : sig val test : int -> bool end)
        struct
            type t = bool
            val id x :t =  x
        end : sig
            type t
            val id : t -> t
        end


    module TestFunctorStatic = FunctorTest(IsZero)(IsZero)


   (* Test the sealing of an argument of a functor *)
    module ArgTest = 
        functor ( Input : ( sig type t = int val test : t -> t val id : t -> t end)) 
            Input : (sig  
                        type t 
                        val test : t -> int
                        val id : t -> t
                    end)

    module TestArgTest = ArgTest(Argument) 

    (* Test the sealing of the functions of a functor argument *)
    module HigherFunctorTest =
        functor ( Input : functor (Input : sig val test : int -> bool end) 
                                  (sig 
                                    type t = bool
                                    val func : int -> bool
                                  end))
                Input : functor (Input : sig val test : int -> bool end)
                        (sig 
                            type t
                            val func : int -> t
                        end) 


end
