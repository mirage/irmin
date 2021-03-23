include Irmin.Export_for_backends

module Int63 = struct
  include Int63

  let t : t Irmin.Type.t =
    let open Irmin.Type in
    (map int64) Int63.of_int64 Int63.to_int64
    |> like ~pp:Int63.pp ~equal:(stage Int63.equal)
         ~compare:(stage Int63.compare)
end
