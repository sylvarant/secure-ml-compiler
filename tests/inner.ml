struct 

    module Outer =
    struct

        val value = true

        module  Inner : (sig val hell : bool end) =
        struct
            val hell = value
        end
    end

end
