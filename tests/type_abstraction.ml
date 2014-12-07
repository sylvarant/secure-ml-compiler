struct

    module Alternate : (sig
        type t;
        val create : t;
    end) =
    struct
        type t = bool
        val create = true
    end

    module Abstract : (sig 
        type t; 
        val func : t -> t; 
        val create : t; 
        module Inner : (sig  val dumb : t -> int end)
    end) =
    struct
        type t = int
        val create = 5
        val func x = x * 2

        module Inner =
        struct
            val dumb y = y
        
        end

    end

end
