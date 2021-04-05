include Irmin.Export_for_backends

let src = Logs.Src.create "irmin.pack" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

module Int63 = struct
  include Optint.Int63

  let t : t Irmin.Type.t =
    let open Irmin.Type in
    (map int64) of_int64 to_int64
    |> like ~pp:Optint.Int63.pp ~equal:(stage Optint.Int63.equal)
         ~compare:(stage Optint.Int63.compare)
end

type int63 = Int63.t [@@deriving irmin]

let ( ++ ) = Int63.add
let ( -- ) = Int63.sub
