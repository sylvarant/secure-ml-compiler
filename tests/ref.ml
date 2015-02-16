struct

    val counter = ref 0

    val value = (counter := 2); !counter 

    val call x : bool = (counter := (!counter) + 1); x

    val crazy x : (ref bool) = (x := false); !x

end
