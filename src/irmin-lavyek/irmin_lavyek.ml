include Irmin.Export_for_backends

let src = Logs.Src.create "irmin.lavyek" ~doc:"Irmin Lavyek store"

module Log = (val Logs.src_log src : Logs.LOG)
module Conf = Conf

let config = Conf.init

module Read_only (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  type key = K.t
  type value = V.t
  type 'a t = { t : Lavyek.t; config : Irmin.config }

  let create ~sw path =
    [%log.debug "create store at %s" (Eio.Path.native_exn path)];
    Lavyek.create ~sw path

  let open_ ~sw path =
    [%log.debug "opening store at %s" (Eio.Path.native_exn path)];
    Lavyek.open_out ~sw path

  let v config =
    let root = Conf.root config in
    let path = Eio.Path.(Conf.fs config / root) in
    let sw = Conf.switch config in
    let fresh = Conf.fresh config in
    let t = if fresh then create ~sw path else open_ ~sw path in
    { t; config }

  let close t =
    [%log.debug "close"];
    Lavyek.close t.t

  let clear _t =
    [%log.debug "clear store"];
    failwith "not implemented list"

  let cast t = (t :> read_write t)
  let batch t f = f (cast t)
  let of_bin_string = Irmin.Type.(unstage (of_bin_string V.t))
  let pp_key = Irmin.Type.pp K.t

  let find t key =
    [%log.debug "find %a" pp_key key];
    Option.map
      (fun v -> Result.get_ok (of_bin_string v))
      (Lavyek.find t.t ~key:(Repr.to_string K.t key))

  let mem t key =
    [%log.debug "mem %a" pp_key key];
    Lavyek.find t.t ~key:(Repr.to_string K.t key) <> None
end

module Append_only (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  include Read_only (K) (V)

  let to_bin_string = Irmin.Type.(unstage (to_bin_string V.t))

  let add t key value =
    [%log.debug "add %a" pp_key key];
    let sync = Conf.sync t.config in
    Lavyek.put t.t ~sync (Repr.to_string K.t key) (to_bin_string value)
end

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  module RO = Read_only (K) (V)
  module W = Irmin.Backend.Watch.Make (K) (V)

  type t = { t : unit RO.t; w : W.t }
  type key = RO.key
  type value = RO.value
  type watch = W.watch

  let watches = W.v ()

  let v config =
    let t = RO.v config in
    { t; w = watches }

  let close t =
    W.clear t.w;
    RO.close t.t

  let find t = RO.find t.t
  let mem t = RO.mem t.t
  let watch_key t = W.watch_key t.w
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w

  let list t =
    [%log.debug "list"];
    let keys, _ = List.split @@ Lavyek.list t.t.t in
    List.map (fun k -> Result.get_ok @@ Repr.of_string K.t k) keys

  let to_bin_string = Irmin.Type.(unstage (to_bin_string V.t))

  let set t key value =
    [%log.debug "update %a" RO.pp_key key];
    let sync = Conf.sync t.t.config in

    Lavyek.put ~sync t.t.t (Repr.to_string K.t key) (to_bin_string value)

  let remove t key =
    [%log.debug "remove %a" RO.pp_key key];
    let sync = Conf.sync t.t.config in
    Lavyek.remove ~sync t.t.t (Repr.to_string K.t key)

  let equal_v_opt = Irmin.Type.(unstage (equal (option V.t)))

  let test_and_set t key ~test ~set:set_value =
    [%log.debug "test_and_set"];
    let updated =
      let v = find t key in
      if equal_v_opt test v then
        let () =
          match set_value with None -> remove t key | Some v -> set t key v
        in
        true
      else false
    in
    if updated then W.notify t.w key set_value;
    updated

  let clear t = RO.clear t.t
end

module Content_addressable = Irmin.Content_addressable.Make (Append_only)
module Maker = Irmin.Maker (Content_addressable) (Atomic_write)
module KV = Irmin.KV_maker (Content_addressable) (Atomic_write)

(* Enforce that {!S} is a sub-type of {!Irmin.Maker}. *)
module Maker_is_a_maker : Irmin.Maker = Maker

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_maker}. *)
module KV_is_a_KV : Irmin.KV_maker = KV
