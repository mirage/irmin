(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Import
open Irmin_server
open Lwt.Syntax
open Lwt.Infix
include Client_intf

exception Continue

module Conf = struct
  include Irmin.Backend.Conf

  let spec = Irmin.Backend.Conf.Spec.v "irmin-client"
  let uri = Irmin.Type.(map string) Uri.of_string Uri.to_string

  let uri =
    Irmin.Backend.Conf.key ~spec "uri" uri
      (Uri.of_string "tcp://127.0.0.1:9181")

  let tls = Irmin.Backend.Conf.key ~spec "tls" Irmin.Type.bool false

  let hostname =
    Irmin.Backend.Conf.key ~spec "hostname" Irmin.Type.string "127.0.0.1"
end

let config ?(tls = false) ?hostname uri =
  let default_host = Uri.host_with_default ~default:"127.0.0.1" uri in
  let config =
    Irmin.Backend.Conf.add (Irmin.Backend.Conf.empty Conf.spec) Conf.uri uri
  in
  let config =
    Irmin.Backend.Conf.add config Conf.hostname
      (Option.value ~default:default_host hostname)
  in
  Irmin.Backend.Conf.add config Conf.tls tls

module Client (I : IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) =
struct
  module C = Command.Make (I) (Codec) (Store)
  open C
  module IO = I

  type t = {
    ctx : IO.ctx;
    config : Conf.t;
    mutable conn : Conn.t;
    mutable closed : bool;
    lock : Lwt_mutex.t;
  }

  let close t =
    t.closed <- true;
    IO.close (t.conn.ic, t.conn.oc)

  let mk_client conf =
    let uri = Conf.get conf Conf.uri in
    let hostname = Conf.get conf Conf.hostname in
    let tls = Conf.get conf Conf.tls in
    let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
    let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
    let client =
      match String.lowercase_ascii scheme with
      | "unix" -> `Unix_domain_socket (`File (Uri.path uri))
      | "tcp" ->
          let port = Uri.port uri |> Option.value ~default:9181 in
          let ip = Ipaddr.of_string_exn addr in
          if not tls then `TCP (`IP ip, `Port port)
          else `TLS (`Hostname hostname, `IP ip, `Port port)
      | "ws" | "wss" -> (
          let port = Uri.port uri |> Option.value ~default:9181 in
          match Ipaddr.of_string addr with
          | Ok ip ->
              if not tls then `Ws (Some (`IP ip, `Port port), Uri.to_string uri)
              else `TLS (`Hostname hostname, `IP ip, `Port port)
          | _ -> `Ws (None, Uri.to_string uri))
      | x -> invalid_arg ("Unknown client scheme: " ^ x)
    in
    client

  let lock t f = Lwt_mutex.with_lock t.lock f [@@inline]

  let send_command_header t (module Cmd : C.CMD) =
    let header = Conn.Request.v_header ~command:Cmd.name in
    Conn.Request.write_header t.conn header

  let recv (t : t) name ty =
    let* res = Conn.Response.read_header t.conn in
    Conn.Response.get_error t.conn res >>= function
    | Some err ->
        [%log.err "Request error: command=%s, error=%s" name err];
        Lwt.return_error (`Msg err)
    | None ->
        let+ x = Conn.read t.conn ty in
        [%log.debug "Completed request: command=%s" name];
        x

  let request_lwt (t : t) (type x y)
      (module Cmd : C.CMD with type res = x and type req = y) (a : y) =
    if t.closed then raise Irmin.Closed
    else
      let name = Cmd.name in
      [%log.debug "Starting request: command=%s" name];
      lock t (fun () ->
          let* () = send_command_header t (module Cmd) in
          let* () = Conn.write t.conn Cmd.req_t a in
          let* () = IO.flush t.conn.oc in
          recv t name Cmd.res_t)

  let request t cmd a = Lwt_eio.run_lwt @@ fun () -> request_lwt t cmd a

  let recv_branch_diff (t : t) =
    let* _status = Conn.Response.read_header t.conn in
    Conn.read t.conn
      (Irmin.Type.pair Store.Branch.t (Irmin.Diff.t Store.commit_key_t))
    >|= Error.unwrap "recv_branch_diff"

  let recv_branch_key_diff (t : t) =
    let* _status = Conn.Response.read_header t.conn in
    Conn.read t.conn (Irmin.Diff.t Store.commit_key_t)
    >|= Error.unwrap "recv_branch_key_diff"
end

module Make (IO : IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) =
struct
  module Client = Client (IO) (Codec) (Store)
  module Command = Command.Make (IO) (Codec) (Store)
  module Conn = Command.Conn
  module Commands = Command.Commands

  let request_lwt = Client.request_lwt
  let request = Client.request

  let rec connect ?ctx config =
    let ctx = Option.value ~default:(Lazy.force IO.default_ctx) ctx in
    let client = Client.mk_client config in
    let* ic, oc = IO.connect ~ctx client in
    let conn = Conn.v ic oc in
    let+ ok = Conn.Handshake.V1.send (module Store) conn in
    if not ok then Error.raise_error "invalid handshake"
    else
      let t =
        Client.{ config; ctx; conn; closed = false; lock = Lwt_mutex.create () }
      in
      t

  and reconnect t =
    let* () = Lwt.catch (fun () -> Client.close t) (fun _ -> Lwt.return_unit) in
    let+ conn = connect ~ctx:t.ctx t.Client.config in
    t.conn <- conn.conn;
    t.closed <- false

  let dup client =
    let* c = connect ~ctx:client.Client.ctx client.Client.config in
    let () = if client.closed then c.closed <- true in
    Lwt.return c

  let uri t = Conf.get t.Client.config Conf.uri

  module X = struct
    open Lwt.Infix
    module Schema = Store.Schema
    module Hash = Store.Hash

    module Contents = struct
      type nonrec 'a t = Client.t

      open Commands.Contents
      module Key = Store.Backend.Contents.Key
      module Val = Store.Backend.Contents.Val
      module Hash = Store.Backend.Contents.Hash

      type key = Key.t
      type value = Val.t
      type hash = Hash.t

      let mem t key = request t (module Mem) key |> Error.unwrap "Contents.mem"

      let find t key =
        request t (module Find) key |> Error.unwrap "Contents.find"

      let add t value =
        request t (module Add) value |> Error.unwrap "Contents.add"

      let unsafe_add t key value =
        request t (module Unsafe_add) (key, value)
        |> Error.unwrap "Contents.unsafe_add"

      let index t hash =
        request t (module Index) hash |> Error.unwrap "Contents.index"

      let batch t f = f t
      let close t = Lwt_eio.run_lwt @@ fun () -> Client.close t

      let merge t =
        let f ~old a b =
          let old = old () in
          match old with
          | Ok old ->
              request t (module Merge) (old, a, b)
              |> Error.unwrap "Contents.merge"
          | Error e -> Error e
        in
        Irmin.Merge.v Irmin.Type.(option Key.t) f
    end

    module Node = struct
      type nonrec 'a t = Client.t

      open Commands.Node
      module Key = Store.Backend.Node.Key
      module Val = Store.Backend.Node.Val
      module Hash = Store.Backend.Node.Hash
      module Path = Store.Backend.Node.Path
      module Metadata = Store.Backend.Node.Metadata
      module Contents = Store.Backend.Node.Contents

      type key = Key.t
      type value = Val.t
      type hash = Hash.t

      let mem t key = request t (module Mem) key |> Error.unwrap "Node.mem"
      let find t key = request t (module Find) key |> Error.unwrap "Node.find"
      let add t value = request t (module Add) value |> Error.unwrap "Node.add"

      let unsafe_add t key value =
        request t (module Unsafe_add) (key, value)
        |> Error.unwrap "Node.unsafe_add"

      let index t hash =
        request t (module Index) hash |> Error.unwrap "Node.index"

      let batch t f = f t
      let close t = Lwt_eio.run_lwt @@ fun () -> Client.close t

      let merge t =
        let f ~old a b =
          let old = old () in
          match old with
          | Ok old ->
              request t (module Merge) (old, a, b) |> Error.unwrap "Node.merge"
          | Error e -> Error e
        in
        Irmin.Merge.v Irmin.Type.(option Key.t) f
    end

    module Node_portable = Store.Backend.Node_portable

    module Commit = struct
      type nonrec 'a t = Client.t

      open Commands.Commit
      module Key = Store.Backend.Commit.Key
      module Val = Store.Backend.Commit.Val
      module Hash = Store.Backend.Commit.Hash
      module Info = Store.Backend.Commit.Info
      module Node = Node

      type key = Key.t
      type value = Val.t
      type hash = Hash.t

      let mem t key = request t (module Mem) key |> Error.unwrap "Commit.mem"
      let find t key = request t (module Find) key |> Error.unwrap "Commit.find"

      let add t value =
        request t (module Add) value |> Error.unwrap "Commit.add"

      let unsafe_add t key value =
        request t (module Unsafe_add) (key, value)
        |> Error.unwrap "Commit.unsafe_add"

      let index t hash =
        request t (module Index) hash |> Error.unwrap "Commit.index"

      let batch t f = f t
      let close t = Lwt_eio.run_lwt @@ fun () -> Client.close t

      let merge t ~info =
        let f ~old a b =
          let old = old () in
          match old with
          | Ok old ->
              request t (module Merge) (info (), (old, a, b))
              |> Error.unwrap "Node.merge"
          | Error e -> Error e
        in
        Irmin.Merge.v Irmin.Type.(option Key.t) f
    end

    module Commit_portable = Store.Backend.Commit_portable

    module Branch = struct
      type nonrec t = Client.t

      open Commands.Branch
      module Key = Store.Backend.Branch.Key
      module Val = Store.Backend.Branch.Val

      type key = Key.t
      type value = Val.t

      let mem t key = request t (module Mem) key |> Error.unwrap "Branch.mem"
      let find t key = request t (module Find) key |> Error.unwrap "Branch.find"

      let set t key value =
        request t (module Set) (key, value) |> Error.unwrap "Branch.set"

      let test_and_set t key ~test ~set =
        request t (module Test_and_set) (key, test, set)
        |> Error.unwrap "Branch.test_and_set"

      let remove t key =
        request t (module Remove) key |> Error.unwrap "Branch.remove"

      let list t = request t (module List) () |> Error.unwrap "Branch.list"

      type watch = t

      let watch t ?init f =
        Lwt_eio.run_lwt @@ fun () ->
        let f key diff = Lwt_eio.run_eio @@ fun () -> f key diff in
        let* t = dup t in
        let* () =
          request_lwt t (module Watch) init >|= Error.unwrap "Branch.watch"
        in
        let rec loop () =
          if t.closed || Conn.is_closed t.conn then Lwt.return_unit
          else
            Lwt.catch
              (fun () ->
                Lwt.catch
                  (fun () -> Client.recv_branch_diff t)
                  (fun _ -> raise Continue)
                >>= fun (key, diff) -> f key diff >>= loop)
              (function _ -> loop ())
        in
        Lwt.async loop;
        Lwt.return t

      let watch_key t key ?init f =
        Lwt_eio.run_lwt @@ fun () ->
        let f x = Lwt_eio.run_eio @@ fun () -> f x in
        let* t = dup t in
        let* () =
          request_lwt t (module Watch_key) (init, key)
          >|= Error.unwrap "Branch.watch_key"
        in
        let rec loop () =
          if t.closed || Conn.is_closed t.conn then Lwt.return_unit
          else
            Lwt.catch
              (fun () ->
                Lwt.catch
                  (fun () -> Client.recv_branch_key_diff t)
                  (fun _ -> raise Continue)
                >>= f
                >>= loop)
              (function _ -> loop ())
        in
        Lwt.async loop;
        Lwt.return t

      let unwatch _t watch =
        Lwt_eio.run_lwt @@ fun () ->
        let* () = Conn.write watch.Client.conn Unwatch.req_t () in
        Client.close watch

      let clear t = request t (module Clear) () |> Error.unwrap "Branch.clear"
      let close t = Lwt_eio.run_lwt @@ fun () -> Client.close t
    end

    module Slice = Store.Backend.Slice

    module Repo = struct
      type nonrec t = Client.t

      let v config = Lwt_eio.run_lwt @@ fun () -> connect config
      let config (t : t) = t.Client.config
      let close (t : t) = Lwt_eio.run_lwt @@ fun () -> Client.close t
      let contents_t (t : t) = t
      let node_t (t : t) = t
      let commit_t (t : t) = t
      let branch_t (t : t) = t
      let batch ?lock:_ (t : t) f = f t t t
    end

    module Remote = Irmin.Backend.Remote.None (Commit.Key) (Store.Branch)
  end

  include Irmin.Of_backend (X)

  let ping t = request t (module Commands.Ping) ()

  let export ?depth t =
    request t (module Commands.Export) depth |> Error.unwrap "export"

  let import t slice =
    request t (module Commands.Import) slice |> Error.unwrap "import"

  let close t = Lwt_eio.run_lwt @@ fun () -> Client.close t

  let connect ?tls ?hostname uri =
    let conf = config ?tls ?hostname uri in
    Repo.v conf

  let request_store store =
    match status store with
    | `Empty -> `Empty
    | `Branch b -> `Branch b
    | `Commit c -> `Commit (Commit.key c)

  module Batch = struct
    module Request_tree = Command.Tree

    type store = t

    type t =
      (Store.path
      * [ `Contents of
          [ `Hash of Store.Hash.t | `Value of Store.contents ]
          * Store.metadata option
        | `Tree of Request_tree.t
        | `Remove ])
      list
    [@@deriving irmin]

    let v () = []
    let remove k t = (k, `Remove) :: t

    let add_value path ?metadata value t =
      (path, `Contents (`Value value, metadata)) :: t

    let add_hash path ?metadata hash t =
      (path, `Contents (`Hash hash, metadata)) :: t

    let add_tree path tree t =
      let tree =
        match Tree.key tree with
        | None ->
            let concrete_tree = Tree.to_concrete tree in
            Request_tree.Concrete concrete_tree
        | Some key -> Request_tree.Key key
      in
      (path, `Tree tree) :: t

    let apply ~info ?(path = Store.Path.empty) store t =
      let repo = repo store in
      let store = request_store store in
      request repo (module Commands.Batch.Apply) ((store, path), info (), t)
      |> Error.unwrap "Batch.apply"
  end

  (* Overrides *)

  module Commit = struct
    include Commit

    module Cache = struct
      module Key = Irmin.Backend.Lru.Make (struct
        type t = commit_key

        let hash = Hashtbl.hash
        let equal = Irmin.Type.(unstage (equal commit_key_t))
      end)

      module Hash = Irmin.Backend.Lru.Make (struct
        type t = hash

        let hash = Hashtbl.hash
        let equal = Irmin.Type.(unstage (equal hash_t))
      end)

      let key : commit Key.t = Key.create 32
      let hash : commit Hash.t = Hash.create 32
    end

    let of_key repo key =
      if Cache.Key.mem Cache.key key then Some (Cache.Key.find Cache.key key)
      else
        let x = of_key repo key in
        Option.iter (Cache.Key.add Cache.key key) x;
        x

    let of_hash repo hash =
      if Cache.Hash.mem Cache.hash hash then
        Some (Cache.Hash.find Cache.hash hash)
      else
        let x = of_hash repo hash in
        Option.iter (Cache.Hash.add Cache.hash hash) x;
        x
  end

  module Contents = struct
    include Contents

    module Cache = struct
      module Hash = Irmin.Backend.Lru.Make (struct
        type t = hash

        let hash = Hashtbl.hash
        let equal = Irmin.Type.(unstage (equal hash_t))
      end)

      let hash : contents Hash.t = Hash.create 32
    end

    let of_hash repo hash =
      if Cache.Hash.mem Cache.hash hash then
        Some (Cache.Hash.find Cache.hash hash)
      else
        let x = of_hash repo hash in
        Option.iter (Cache.Hash.add Cache.hash hash) x;
        x
  end

  let clone ~src ~dst =
    let repo = repo src in
    let repo = Lwt_eio.run_lwt @@ fun () -> dup repo in
    let () =
      match Head.find src with
      | None -> Branch.remove repo dst
      | Some h -> Branch.set repo dst h
    in
    of_branch repo dst

  let request_store store =
    match status store with
    | `Empty -> `Empty
    | `Branch b -> `Branch b
    | `Commit c -> `Commit (Commit.key c)

  let mem store path =
    let repo = repo store in
    request repo (module Commands.Store.Mem) (request_store store, path)
    |> Error.unwrap "mem"

  let mem_tree store path =
    let repo = repo store in
    request repo (module Commands.Store.Mem_tree) (request_store store, path)
    |> Error.unwrap "mem_tree"

  let find store path =
    let repo = repo store in
    request repo (module Commands.Store.Find) (request_store store, path)
    |> Error.unwrap "find"

  let remove_exn ?clear ?retries ?allow_empty ?parents ~info store path =
    let parents = Option.map (List.map (fun c -> Commit.hash c)) parents in
    let repo = repo store in
    request repo
      (module Commands.Store.Remove)
      ( ((clear, retries), (allow_empty, parents)),
        (request_store store, path),
        info () )
    |> Error.unwrap "remove"

  let remove ?clear ?retries ?allow_empty ?parents ~info store path =
    let x = remove_exn ?clear ?retries ?allow_empty ?parents ~info store path in
    Ok x

  let find_tree store path =
    let repo = repo store in
    let concrete =
      request repo (module Commands.Store.Find_tree) (request_store store, path)
      |> Error.unwrap "find_tree"
    in
    Option.map Tree.of_concrete concrete
end
