open! Import
module IO = IO.Unix

module type Args = sig
  module Version : Version.S
  module Hash : Irmin.Hash.S
  module Index : Pack_index.S with type key := Hash.t
  module Inode : Inode.S with type key := Hash.t
  module Dict : Pack_dict.S
  module Contents : Pack_value.S
  module Commit : Pack_value.S
end

module Make (Args : Args) : sig
  val run : ?output:string -> Irmin.config -> unit
end = struct
  open Args

  let pp_hash = Irmin.Type.pp Hash.t
  let decode_key = Irmin.Type.(unstage (decode_bin Hash.t))
  let decode_kind = Irmin.Type.(unstage (decode_bin Pack_value.Kind.t))

  let decode_buffer ~progress ~total pack dict index =
    let decode_len buf (kind : Pack_value.Kind.t) =
      try
        let len =
          match kind with
          | Contents ->
              fst
                (Contents.decode_bin
                   ~dict:(fun _ -> assert false)
                   ~hash:(fun _ -> assert false)
                   buf 0)
          | Commit ->
              fst
                (Commit.decode_bin
                   ~dict:(fun _ -> assert false)
                   ~hash:(fun _ -> assert false)
                   buf 0)
          | Node | Inode ->
              let hash off =
                let buf = IO.read_buffer ~chunk:Hash.hash_size ~off pack in
                decode_key buf 0 |> snd
              in
              let dict = Dict.find dict in
              Inode.decode_bin ~hash ~dict buf 0
        in
        Some len
      with
      | Invalid_argument msg when msg = "index out of bounds" -> None
      | Invalid_argument msg when msg = "String.blit / Bytes.blit_string" ->
          None
    in
    let decode_entry buf off =
      let off_k, k = decode_key buf 0 in
      assert (off_k = Hash.hash_size);
      let off_m, kind = decode_kind buf off_k in
      assert (off_m = Hash.hash_size + 1);
      match decode_len buf kind with
      | Some len ->
          let new_off = off ++ Int63.of_int len in
          Log.debug (fun l ->
              l "k = %a (off, len, kind) = (%a, %d, %a)" pp_hash k Int63.pp off
                len Pack_value.Kind.pp kind);
          Index.add index k (off, len, kind);
          progress (Int63.of_int len);
          Some new_off
      | None -> None
    in
    let rec read_and_decode ?(retries = 1) off =
      Log.debug (fun l ->
          l "read_and_decode retries %d off %a" retries Int63.pp off);
      let chunk = 64 * 10 * retries in
      let buf = IO.read_buffer ~chunk ~off pack in
      match decode_entry buf off with
      | Some new_off -> new_off
      | None ->
          (* the biggest entry in a tezos store is a blob of 54801B *)
          if retries > 90 then
            failwith "too many retries to read data, buffer size = 57600B"
          else (read_and_decode [@tailcall]) ~retries:(retries + 1) off
    in
    let rec read_buffer off =
      if off >= total then ()
      else
        let new_off = read_and_decode off in
        (read_buffer [@tailcall]) new_off
    in
    read_buffer Int63.zero

  let run ?output config =
    if Conf.readonly config then raise S.RO_not_allowed;
    Log.info (fun l -> l "[%s] reconstructing index" (Conf.root config));
    let root = Conf.root config in
    let dest = match output with Some path -> path | None -> root in
    let log_size = Conf.index_log_size config in
    let index = Index.v ~fresh:true ~readonly:false ~log_size dest in
    let pack_file = Filename.concat root "store.pack" in
    let pack =
      IO.v ~fresh:false ~readonly:true ~version:(Some Version.version) pack_file
    in
    let dict = Dict.v ~fresh:false ~readonly:true root in
    let total = IO.offset pack in
    let bar, progress =
      Utils.Progress.counter ~total ~sampling_interval:100
        ~message:"Reconstructing index" ~pp_count:Utils.pp_bytes ()
    in
    decode_buffer ~progress ~total pack dict index;
    Index.close index;
    IO.close pack;
    Dict.close dict;
    Utils.Progress.finalise bar
end
