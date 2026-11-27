(* Races the unguarded [Buffer.t] and atomic counter in
   [Append_only_file.rw_perm] (src/irmin-pack/io/append_only_file.ml).

   N domains call [Ao.append_exn] concurrently on the same [Ao.t].
   The function does [Buffer.add_string] without synchronisation, then
   an [Atomic.fetch_and_add] on [buf_length] — the TOCTOU and the
   Buffer mutation both surface to TSan. *)

module Io = Irmin_pack_unix.Io.Unix
module Errs = Irmin_pack_io.Io_errors.Make (Io)
module Ao = Irmin_pack_io.Append_only_file.Make (Io) (Errs)

let run ~env ~iter =
  Eio.Switch.run @@ fun sw ->
  let dmgr = Eio.Stdenv.domain_mgr env in
  let path = Stress_common.scratch_file ~env "stress_ao_buf.data" in
  let ao =
    match Ao.create_rw ~sw ~path ~overwrite:true with
    | Ok ao -> ao
    | Error _ -> failwith "stress_ao_buf: create_rw failed"
  in
  let worker () =
    for _ = 1 to iter do
      Ao.append_exn ao "xxxxxxxx"
    done
  in
  Stress_common.domains_spawn ~dmgr ~nb:2 worker;
  ignore (Ao.flush ao);
  ignore (Ao.close ao)
