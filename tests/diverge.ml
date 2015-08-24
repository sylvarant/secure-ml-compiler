struct 

    module Inner =
    struct

        val terminate x = (exit x)

        val recurse = (fun y : bool = 
            letrec div : bool -> bool = (fun x : bool = (if x then true else (div true)))
            in (div y))

    end

end
