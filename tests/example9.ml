struct

    module Main : (sig val public : int -> int end)  =
    struct

        val secret = 10 

        val public y = let x = 5 in (( x + y ) * secret)

    end

end
