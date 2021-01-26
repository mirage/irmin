type read = Perms.read
type write = Perms.write
type read_write = Perms.read_write

let ( >>= ) = Lwt.Infix.( >>= )
let ( >|= ) = Lwt.Infix.( >|= )
let ( let* ) = ( >>= )
let ( let+ ) = ( >|= )
