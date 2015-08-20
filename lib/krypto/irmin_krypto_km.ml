


module type KEY_MANAGEMENT = sig

    val key_data: Cstruct.t
    val key_header: Cstruct.t
   
end



module Make : KEY_MANAGEMENT = struct

    let key_data =  Cstruct.(of_string "12345678901234567890123456789012")   
    let key_header =  Cstruct.(of_string "97856341209785634120978563412021")
   
  end
