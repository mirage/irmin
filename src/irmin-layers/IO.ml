let src = Logs.Src.create "irmin.layers.io" ~doc:"IO for irmin-layers"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type t

  val v : string -> bytes -> t

  val write : t -> bytes -> unit

  val read : t -> bytes -> unit

  val close : t -> unit
end

module Unix : S = struct
  type t = { file : string; fd : Unix.file_descr }

  let write t buf =
    let off = Unix.lseek t.fd 0 Unix.SEEK_SET in
    assert (off = 0);
    let len = Bytes.length buf in
    let n = Unix.write t.fd buf 0 len in
    assert (n = len)

  let read t buf =
    let off = Unix.lseek t.fd 0 Unix.SEEK_SET in
    assert (off = 0);
    let n = Unix.read t.fd buf 0 1 in
    assert (n = 1)

  let close t = Unix.close t.fd

  let v file init =
    match Sys.file_exists file with
    | false ->
        let fd = Unix.openfile file Unix.[ O_CREAT; O_RDWR; O_CLOEXEC ] 0o644 in
        let t = { file; fd } in
        write t init;
        t
    | true ->
        let fd = Unix.openfile file Unix.[ O_EXCL; O_RDWR; O_CLOEXEC ] 0o644 in
        { file; fd }
end
