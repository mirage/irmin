let ( >> ) f g x = g (f x)

module Identity = struct
  type 'a t = 'a

  let id x = x

  let map = id

  let return = id

  let lift_bin = id

  let bind = id

  module Infix = struct
    let ( >|= ) = ( |> )

    let ( >=| ) = ( >> )

    let ( >>= ) = ( |> )

    let ( >=> ) = ( >> )
  end

  module Syntax = struct
    let ( let+ ) = ( |> )

    let ( let* ) = ( |> )
  end
end

module Option = struct
  type 'a t = 'a option

  let map f = function Some x -> Some (f x) | None -> None

  let return x = Some x

  let lift_bin f a b =
    match (a, b) with Some a, Some b -> Some (f a b) | _ -> None

  let bind f = function Some x -> f x | None -> None

  module Infix = struct
    let ( >|= ) x f = map f x

    let ( >=| ) f g x = map g (f x)

    let ( >>= ) x f = bind f x

    let ( >=> ) f g x = bind g (f x)
  end

  module Syntax = struct
    let ( let+ ) x f = map f x

    let ( let* ) x f = bind f x
  end
end
