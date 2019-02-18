open Lwt.Infix

let src = Logs.Src.create "irmin.fdb" ~doc:"Irmin FoundationDB store"
module Log = (val Logs.src_log src : Logs.LOG)

module type IO = Fdb.IO with type 'a t = 'a Lwt.t

module Read_only (IO : IO) (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
  module F = struct
    include Fdb.Make(IO)

    exception Error of string

    let fail_if_error x =
      x >>= function
      | Ok ok -> Lwt.return ok
      | Error err -> Lwt.fail (Error (Error.to_string err))
  end

  type key = K.t
  type value = V.t
  type 'a t = {
    db : F.Database.t;
    prefix : string;
  }
  let v prefix config =
    let open F.Infix in
    let module C = Irmin.Private.Conf in
    let prefix = match C.get config C.root with
      | Some root -> root ^ ":" ^ prefix ^ ":"
      | None -> prefix ^ ":"
    in
    begin
      F.open_database () >>|? fun db ->
      { db; prefix }
    end
    |> F.fail_if_error

  let cast t = (t :> [`Read | `Write] t)
  let batch t f = f (cast t)

  let pp_key = Irmin.Type.pp K.t

  let value v =
    match Irmin.Type.decode_bin V.t v with
    | Ok v           -> Some v
    | Error (`Msg e) ->
      Log.err (fun l -> l "Irmin_fdb.value %s" e);
      None

  let prefixed_key t k =
    let key_string = Irmin.Type.to_string K.t k in
    Fdb.Tuple.pack [`Bytes t.prefix; `Bytes key_string]

  let find t key =
    let open F.Infix in
    let key = prefixed_key t key in
    Log.debug (fun f -> f "find %s" (String.escaped key));
    begin
      F.Database.get t.db ~key
      >>|? function
      | None -> None
      | Some x -> value x
    end
    |> F.fail_if_error

  let mem t key =
    let open F.Infix in
    let key = prefixed_key t key in
    Log.debug (fun f -> f "mem %s" (String.escaped key));
    let key_selector = F.Key_selector.first_greater_or_equal key in
    begin
      F.Database.get_key t.db ~key_selector
      >>|? String.equal ""
    end
    |> F.fail_if_error

end

module Append_only (IO : IO) (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

  include Read_only(IO)(K)(V)

  let v config =
    v "obj" config

  let add t key value =
    let key = prefixed_key t key in
    Log.debug (fun f -> f "add -> %s" (String.escaped key));
    let value = Irmin.Type.encode_bin V.t value in
    F.Database.set t.db ~key ~value
    |> F.fail_if_error

end

module Atomic_write (IO : IO) (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

  module RO = Read_only(IO)(K)(V)
  module F = RO.F
  module W = Irmin.Private.Watch.Make(K)(V)

  type t = { t: unit RO.t; w: W.t }
  type key = RO.key
  type value = RO.value
  type watch = W.watch

  let watches = W.v ()

  let v config =
    RO.v "data" config >>= fun t ->
    Lwt.return { t; w = watches }

  let find t = RO.find t.t
  let mem t = RO.mem t.t

  let watch_key t key = W.watch_key t.w key
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w

  let list t =
    let open F.Infix in
    Log.debug (fun f -> f "list");
    let prefix = F.Key_selector.first_greater_than (Fdb.Tuple.pack [`Bytes t.t.RO.prefix]) in
    F.Database.with_tx t.t.RO.db ~f:(fun tx ->
      F.Transaction.get_range_prefix tx ~prefix >>=? fun range_result ->
      F.Range_result.to_list range_result
    )
    >>|? List.map (fun kv ->
      F.Key_value.key kv
      |> Fdb.Tuple.unpack
      |> (function [`Bytes _prefix; `Bytes k] -> k | _ -> failwith "Invalid key")
      |> Irmin.Type.of_string K.t
      |> function | Ok x -> x | Error (`Msg y) -> failwith y
    )
    |> F.fail_if_error

  let set t key value =
    let k = RO.prefixed_key t.t key in
    Log.debug (fun f -> f "update %s" (String.escaped k));
    let v = Irmin.Type.encode_bin V.t value in
    F.Database.set t.t.RO.db ~key:k ~value:v
    |> F.fail_if_error >>= fun () ->
    W.notify t.w key (Some value)

  let remove t key =
    let k = RO.prefixed_key t.t key in
    Log.debug (fun f -> f "remove %s" (String.escaped k));
    F.Database.clear t.t.RO.db ~key:k;
    |> F.fail_if_error >>= fun () ->
    W.notify t.w key None

  let test_and_set t key ~test ~set =
    let open F.Infix in
    let k = RO.prefixed_key t.t key in
    Log.debug (fun f -> f "test_and_set %s" (String.escaped k));
    F.Database.with_tx t.t.RO.db ~f:(fun tx ->
      F.Transaction.get tx ~key:k >>=? fun v ->
      let v = match v with None -> None | Some v -> RO.value v in
      if Irmin.Type.(equal (option V.t)) test v then (
        let () = match set with
          | None   -> F.Transaction.clear tx ~key:k
          | Some set ->
            let value = Irmin.Type.encode_bin V.t set in
            F.Transaction.set tx ~key:k ~value
        in
        Lwt.return (Ok true)
      ) else
        Lwt.return (Ok false)
    )
    |> F.fail_if_error >>= fun updated ->
    (if updated then W.notify t.w key set else Lwt.return_unit) >>= fun () ->
    Lwt.return updated
end

let config () = Irmin.Private.Conf.empty

module Make (IO : IO)
    (M: Irmin.Metadata.S)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S)
    (H: Irmin.Hash.S)
= struct
  module AO = Append_only(IO)
  module AW = Atomic_write(IO)
  include Irmin.Make(Irmin_chunk.Content_addressable(AO))(AW)(M)(C)(P)(B)(H)
end

module KV (IO : IO) (C: Irmin.Contents.S) =
  Make
    (IO)
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
