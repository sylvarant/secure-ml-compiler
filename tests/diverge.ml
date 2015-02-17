struct 

    module Inner =
    struct

        (*val terminate = exit true*)

        val recurse = (fun y : bool = 
            letrec div : bool -> bool = (fun x : bool = (if x then true else (div true)))
            in (div y))

    end

end
