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

open Lwt.Syntax
open Irmin_client_unix
open Util
module Info = Info (Client.Info)

let info = Info.v

let () =
  let style_renderer = `None in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter (Logs_fmt.reporter ())

module type R = sig
  val uri : Uri.t
  val kind : string
end

module Make (R : R) = struct
  let config = Irmin_client_unix.config R.uri
  let client = Lwt_main.run (Client.Repo.v config)
  let clean ~config:_ = Client.Branch.remove client "main"

  module X = Irmin_mem.KV.Make (Irmin.Contents.String)
  module Store = Irmin_client_unix.Make (X)

  let suite =
    Irmin_test.Suite.create_generic_key ~name:R.kind
      ~store:(module Store)
      ~config ~clean ()
end

let unix_server = run_server `Unix_domain
let tcp_server = run_server `Tcp
let websocket_server = run_server `Websocket

(* A server on an irmin-pack store. An inode holds at most 32 entries, so a
   node with more entries is sent as a root inode which points to child
   inodes. *)
module Pack = struct
  module Conf = struct
    let entries = 32
    let stable_hash = 256
    let contents_length_header = Some `Varint
    let inode_child_order = `Seeded_hash
    let forbid_empty_dir_persistence = true
  end

  module KV = Irmin_pack_unix.KV (Conf)
  module Store = KV.Make (Irmin.Contents.String)
  module Server = Irmin_server_unix.Make (Store)
  module Node = Store.Backend.Node

  let config ?(dir = "test-pack") ~fresh () =
    Irmin_pack.config ~fresh (Filename.concat (Unix.getcwd ()) dir)

  (* Store a node with 100 entries in a new store in [dir]. Return its hash,
     and its value as the server decodes it. *)
  let store_node ~dir contents =
    Lwt_main.run
    @@
    let* repo = Store.Repo.v (config ~dir ~fresh:true ()) in
    let entries =
      List.init 100 (fun i -> (string_of_int i, `Contents (contents, ())))
    in
    let* main = Store.main repo in
    let* () =
      Store.set_tree_exn ~info:Store.Info.none main [ "dir" ]
        (Store.Tree.of_concrete (`Tree entries))
    in
    let* tree = Store.get_tree main [ "dir" ] in
    let key =
      match Store.Tree.key tree with
      | Some (`Node key) -> key
      | _ -> Alcotest.fail "no key for the node"
    in
    let* value = Node.find (Store.Backend.Repo.node_t repo) key in
    let value =
      match value with
      | Some v ->
          Irmin.Type.(unstage (to_bin_string Node.Val.t)) v
          |> Irmin.Type.(unstage (of_bin_string Node.Val.t))
          |> Result.get_ok
      | None -> Alcotest.fail "node not found"
    in
    let+ () = Store.Repo.close repo in
    (Node.Key.to_hash key, value)

  (* A node stored by the server. *)
  let node = store_node ~dir:"test-pack" "x"

  (* A node whose child inodes are not stored by the server. *)
  let unknown_node = store_node ~dir:"test-pack-other" "y"

  (* [value], a root inode of tag Tree, with [length] as its number of
     entries. In its binary encoding, this number follows the hash, the root
     flag, the tag and the depth, which take one byte each here. *)
  let with_length length value =
    let encode = Irmin.Type.(unstage (to_bin_string Node.Val.t)) in
    let decode = Irmin.Type.(unstage (of_bin_string Node.Val.t)) in
    let hash_size =
      String.length
        (Irmin.Type.(unstage (to_bin_string Store.Hash.t))
           (Store.Hash.hash (fun f -> f "")))
    in
    let bytes = Bytes.of_string (encode value) in
    let offset = hash_size + 3 in
    Alcotest.(check int)
      "length in the encoding" (Node.Val.length value)
      (Char.code (Bytes.get bytes offset));
    Bytes.set bytes offset (Char.chr length);
    let value = Bytes.to_string bytes |> decode |> Result.get_ok in
    Alcotest.(check int) "new length" length (Node.Val.length value);
    value

  let uri = unix_uri "test-pack.sock"

  let (_ : int) =
    let serve uri =
      let* server = Server.v ~uri (config ~fresh:false ()) in
      Server.serve server
    in
    fork_server ~serve uri
end

module Unix_socket = Make (struct
  let kind, _, uri = unix_server
end)

module Tcp_socket = Make (struct
  let kind, _, uri = tcp_server
end)

module Websocket = Make (struct
  let kind, _, uri = websocket_server
end)

let _, _, uri = unix_server
let config = Irmin_client_unix.config uri
let client = Lwt_main.run (Client.Repo.v config)
let client () = Client.dup client

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ty t =
  Alcotest.testable
    (Fmt.using (Irmin.Type.to_string t) Fmt.string)
    (fun a b -> Irmin.Type.(unstage (equal t)) a b)

let ping () =
  let open Client in
  let* client = client () in
  Logs.debug (fun l -> l "BEFORE PING");
  let+ r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

(* Send [command] with the request [req] of type [req_t], and read a
   response of type [res_t]. *)
let request conn command req_t req res_t =
  let* () = send_command conn command in
  let* () = Conn.write conn req_t req in
  let* header = Conn.Response.read_header conn in
  let* err = Conn.Response.get_error conn header in
  match err with
  | Some err -> Lwt.return (Error err)
  | None -> (
      let+ res = Conn.read conn res_t in
      match res with Ok x -> Ok x | Error (`Msg err) -> Error err)

let unsafe_add conn command value_t key_t hash value =
  request conn command
    (Irmin.Type.pair Store.Hash.t value_t)
    (hash, value) key_t

let refused command =
  Error ("ERROR " ^ command ^ ": the hash is not the hash of the value")

let with_raw_connection uri f =
  let* conn = raw_connection uri in
  Lwt.finalize (fun () -> f conn) (fun () -> Lwt_io.close conn.ic)

(* The server must not store a value under a hash chosen by the client. *)
let unsafe_add_hash () =
  let open Store.Backend in
  let check msg expected key_t r =
    Alcotest.(check (result (ty key_t) string)) msg expected r
  in
  with_raw_connection uri @@ fun conn ->
  let command = "contents.unsafe_add" in
  let contents_hash = Contents.Hash.hash "unsafe_add value" in
  let add = unsafe_add conn command Store.contents_t Contents.Key.t in
  let* r = add contents_hash "evil" in
  check "wrong contents hash" (refused command) Contents.Key.t r;
  let* r =
    request conn "contents.find" Contents.Key.t contents_hash
      Irmin.Type.(option Store.contents_t)
  in
  Alcotest.(check (result (option string) string)) "not stored" (Ok None) r;
  let* r = add contents_hash "unsafe_add value" in
  check "right contents hash" (Ok contents_hash) Contents.Key.t r;
  let command = "node.unsafe_add" in
  let node = Node.Val.of_list [ ("a", `Contents (contents_hash, ())) ] in
  let node_hash = Node.Hash.hash node in
  let add = unsafe_add conn command Node.Val.t Node.Key.t in
  let* r = add contents_hash node in
  check "wrong node hash" (refused command) Node.Key.t r;
  let* r = add node_hash node in
  check "right node hash" (Ok node_hash) Node.Key.t r;
  let command = "commit.unsafe_add" in
  let commit =
    Commit.Val.v ~info:Commit.Info.empty ~node:node_hash ~parents:[]
  in
  let commit_hash = Commit.Hash.hash commit in
  let add = unsafe_add conn command Commit.Val.t Commit.Key.t in
  let* r = add node_hash commit in
  check "wrong commit hash" (refused command) Commit.Key.t r;
  let+ r = add commit_hash commit in
  check "right commit hash" (Ok commit_hash) Commit.Key.t r

(* The server must be able to check the hash of a node of irmin-pack sent
   as a root inode which points to child inodes. *)
let unsafe_add_pack_node () =
  with_raw_connection Pack.uri @@ fun conn ->
  let command = "node.unsafe_add" in
  let add (hash, value) =
    request conn command
      (Irmin.Type.pair Pack.Store.Hash.t Pack.Node.Val.t)
      (hash, value) Pack.Node.Key.t
    |> Lwt.map (Result.map Pack.Node.Key.to_hash)
  in
  let check msg expected r =
    Alcotest.(check (result (ty Pack.Store.Hash.t) string)) msg expected r
  in
  let error msg = Error ("ERROR " ^ command ^ ": " ^ msg) in
  let hash, value = Pack.node in
  let* r = add (Pack.Store.Backend.Contents.Hash.hash "x", value) in
  check "wrong hash" (refused command) r;
  let* r = add (hash, value) in
  check "right hash" (Ok hash) r;
  let* r = add Pack.unknown_node in
  check "unknown child inodes"
    (error "cannot find a child inode in the store")
    r;
  let+ r = add (hash, Pack.with_length 1 value) in
  check "more child inodes than entries" (error "too many child inodes") r

let misc =
  [
    ("ping", `Quick, ping);
    ("unsafe_add and hash", `Quick, unsafe_add_hash);
    ("unsafe_add of a big irmin-pack node", `Quick, unsafe_add_pack_node);
  ]

let misc = [ ("misc", misc) ]

let () =
  let slow = Sys.getenv_opt "SLOW" |> Option.is_some in
  let only = Sys.getenv_opt "ONLY" in
  let tests =
    match only with
    | Some "ws" -> [ (`Quick, Websocket.suite) ]
    | Some "tcp" -> [ (`Quick, Tcp_socket.suite) ]
    | Some "unix" -> [ (`Quick, Unix_socket.suite) ]
    | Some s -> failwith ("Invalid selection: " ^ s)
    | None ->
        [
          (`Quick, Unix_socket.suite);
          (`Quick, Tcp_socket.suite);
          (`Quick, Websocket.suite);
        ]
  in
  Lwt_main.run
    (Irmin_test.Store.run "irmin-server" ~sleep:Lwt_unix.sleep ~slow ~misc tests)
