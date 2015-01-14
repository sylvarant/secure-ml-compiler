(* test that the attacker can apply a secure functor
as an argument to another secure functor *)
struct

    module Argument = 
    struct
        val func x : int =  x + 1 
    end

    module IdF = 
      functor ( Arg : sig val func : int -> int end) Arg    

    (* higher order Identity functor *)
    module IdHighF =
        functor ( Func : functor (Input : sig val func : int -> int end) 
                                 (sig val func : int -> int end)) Func 

    module SimpleAppF = 
        functor ( Func : functor (Input : sig val func : int -> int end) 
                                 (sig val func : int -> int end)) Func (Argument)

    module InnerAppF = 
        functor ( Func : functor (Input : sig val func : int -> int end) 
                                 (sig val func : int -> int end)) 
        struct

            module Argument = struct
                val func x : int = x + 5
            end

            module Result = Func (Argument) 
        end
end

