type flow = Conduit_lwt_unix.flow
type ic = Conduit_lwt_unix.ic
type oc = Conduit_lwt_unix.oc

exception Timeout = Lwt_unix.Timeout

let is_closed (x : ic) = Lwt_io.is_closed x
let write_int64_be = Lwt_io.BE.write_int64
let read_int64_be = Lwt_io.BE.read_int64
let flush = Lwt_io.flush
let write = Lwt_io.write
let read_into_exactly = Lwt_io.read_into_exactly
let write_char = Lwt_io.write_char
let read_char = Lwt_io.read_char
let with_timeout = Lwt_unix.with_timeout
let time = Unix.time
