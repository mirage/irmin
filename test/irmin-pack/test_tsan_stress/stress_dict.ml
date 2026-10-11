(* Races the two unguarded [Hashtbl.t]s and [mutable last_refill_offset]
   in src/irmin-pack/io/dict.ml.

   One writer domain loops [Dict.index] (which appends and mutates both
   tables via [Hashtbl.add]); readers concurrently loop [Dict.find]. *)

module Io = Irmin_pack_unix.Io.Unix
module Dict = Irmin_pack_io.Dict.Make (Io)

let run ~env ~iter =
  Eio.Switch.run @@ fun sw ->
  let dmgr = Eio.Stdenv.domain_mgr env in
  let path = Stress_common.scratch_file ~env "stress_dict.data" in
  let dict =
    match Dict.create_rw ~sw ~overwrite:true ~path with
    | Ok d -> d
    | Error _ -> failwith "stress_dict: create_rw failed"
  in
  let writer () =
    for i = 1 to iter do
      let _ = Dict.index dict (Printf.sprintf "str-%d" i) in
      ()
    done
  in
  let reader () =
    for i = 1 to iter do
      let _ = Dict.find dict i in
      ()
    done
  in
  Stress_common.domains_run ~dmgr [ writer; reader ];
  ignore (Dict.close dict)
