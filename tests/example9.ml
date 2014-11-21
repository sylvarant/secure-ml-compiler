struct

    module Main : (sig val public : int -> int; module Inner : (sig val innerf : int end)  end)  =
    struct

        val secret = 10 

        val public y = let x = 5 in (( x + y ) * secret)

        module Inner : (sig val innerf : int end) = 
        struct
            val ihave = secret

            val ihavetwice = secret

            val innerf = (ihave + ihavetwice)
        end

        module IgnoreMe = 
        struct
            val secret = 20
        end

    end

    val indirection = Main.public

end
