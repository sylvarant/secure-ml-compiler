(* test that the attacker can apply a secure functor
as an argument to another secure functor *)

struct

    module Argument = 
    struct
        val func x : int =  x + 1 
    end

    module SimpleFunctor = 
      functor ( Arg : sig val func : int -> int end) Arg    

end

