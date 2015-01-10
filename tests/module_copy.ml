struct

    module One = struct
        val value = (1,false)
    end

    module Two = One

    val test = Two.value

end

