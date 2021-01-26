include Irmin.Export_for_backends

let ( >>= ) = Lwt.Infix.( >>= )
let ( >|= ) = Lwt.Infix.( >|= )
let ( let* ) = ( >>= )
let ( let+ ) = ( >|= )
