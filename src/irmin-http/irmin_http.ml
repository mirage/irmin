(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Common
open Astring
open! Import

let src = Logs.Src.create "irmin.http" ~doc:"Irmin HTTP REST interface"

module Log = (val Logs.src_log src : Logs.LOG)
module T = Irmin.Type

module Conf = struct
  include Irmin.Private.Conf.Make ()

  module Key = struct
    (* ~uri *)
    let uri =
      key ~docv:"URI" ~doc:"Location of the remote store." "uri"
        Irmin.Type.(option uri)
        None
  end
end

let config x config = Conf.(add config Key.uri (Some x))

let uri_append t path =
  match Uri.path t :: path with
  | [] -> t
  | path ->
      let buf = Buffer.create 10 in
      List.iter
        (function
          | "" -> ()
          | s ->
              if s.[0] <> '/' then Buffer.add_char buf '/';
              Buffer.add_string buf s)
        path;
      let path = Buffer.contents buf in
      Uri.with_path t path

let err_no_uri () = invalid_arg "Irmin_http.create: No URI specified"

let get_uri config =
  match Conf.(get config Key.uri) with None -> err_no_uri () | Some u -> u

let invalid_arg fmt = Fmt.kstrf Lwt.fail_invalid_arg fmt

exception Escape of ((int * int) * (int * int)) * Jsonm.error

let () =
  Printexc.register_printer (function
    | Escape ((start, end_), err) ->
        Fmt.kstr
          (fun s -> Some s)
          "Escape ({ start = %a; end = %a; error = %a })"
          Fmt.(Dump.pair int int)
          start
          Fmt.(Dump.pair int int)
          end_ Jsonm.pp_error err
    | _ -> None)

let json_stream (stream : string Lwt_stream.t) : Jsonm.lexeme list Lwt_stream.t
    =
  let d = Jsonm.decoder `Manual in
  let rec lexeme () =
    match Jsonm.decode d with
    | `Lexeme l -> Lwt.return l
    | `Error e -> Lwt.fail (Escape (Jsonm.decoded_range d, e))
    | `End -> assert false
    | `Await -> (
        Lwt_stream.get stream >>= function
        | None -> Lwt.fail (Escape (Jsonm.decoded_range d, `Expected `Value))
        | Some str ->
            Jsonm.Manual.src d (Bytes.of_string str) 0 (String.length str);
            lexeme ())
  in
  let lexemes e =
    let lexemes = ref [] in
    let objs = ref 0 in
    let arrs = ref 0 in
    let rec aux () =
      let* l = lexeme e in
      lexemes := l :: !lexemes;
      let () =
        match l with
        | `Os -> incr objs
        | `As -> incr arrs
        | `Oe -> decr objs
        | `Ae -> decr arrs
        | `Name _ | `Null | `Bool _ | `String _ | `Float _ -> ()
      in
      if !objs > 0 || !arrs > 0 then aux () else Lwt.return_unit
    in
    let+ () = aux () in
    List.rev !lexemes
  in
  let open_stream () =
    lexeme () >>= function
    | `As -> Lwt.return_unit
    | _ -> Lwt.fail (Escape (Jsonm.decoded_range d, `Expected (`Aval true)))
  in
  let opened = ref false in
  let open_and_get () =
    if not !opened then (
      open_stream () >>= fun () ->
      opened := true;
      lexemes () >|= Option.some)
    else lexemes () >|= Option.some
  in
  Lwt_stream.from open_and_get

let of_json = Irmin.Type.of_json_string
let to_json = Irmin.Type.to_json_string

module Helper (Client : Cohttp_lwt.S.Client) :
  S.Helper with type ctx = Client.ctx = struct
  type ctx = Client.ctx

  let err_bad_version v =
    invalid_arg "bad server version: expecting %s, but got %s" Irmin.version
      (match v with None -> "<none>" | Some v -> v)

  let check_version r =
    match Cohttp.Header.get (Cohttp.Response.headers r) irmin_version with
    | None -> err_bad_version None
    | Some v ->
        if v <> Irmin.version then err_bad_version (Some v) else Lwt.return_unit

  let is_success r =
    match Cohttp.Response.status r with `OK -> true | _ -> false

  let map_string_response parse (r, b) =
    check_version r >>= fun () ->
    let* b = Cohttp_lwt.Body.to_string b in
    if is_success r then
      match parse b with
      | Ok x -> Lwt.return x
      | Error (`Msg e) ->
          Lwt.fail_with (Fmt.strf "Error while parsing %S: %s" b e)
    else Lwt.fail_with ("Server error: " ^ b)

  let map_stream_response t (r, b) =
    check_version r >>= fun () ->
    if not (is_success r) then
      let* b = Cohttp_lwt.Body.to_string b in
      Lwt.fail_with ("Server error: " ^ b)
    else
      let stream = Cohttp_lwt.Body.to_stream b in
      let stream = json_stream stream in
      let stream =
        let aux j =
          match T.decode_json_lexemes t j with
          | Error (`Msg e) -> Lwt.fail_with e
          | Ok c -> Lwt.return c
        in
        Lwt_stream.map_s aux stream
      in
      Lwt.return stream

  let headers ~keep_alive () =
    let keep_alive =
      if keep_alive then [ ("Connection", "Keep-Alive") ] else []
    in
    Cohttp.Header.of_list
      ([ (irmin_version, Irmin.version); ("Content-type", "application/json") ]
      @ keep_alive)

  let map_call meth t ctx ~keep_alive ?body path fn =
    let uri = uri_append t path in
    let body = match body with None -> None | Some b -> Some (`String b) in
    let headers = headers ~keep_alive () in
    Log.debug (fun f ->
        f "%s %s" (Cohttp.Code.string_of_method meth) (Uri.path uri));
    Lwt.catch
      (fun () -> Client.call ?ctx meth ~headers ?body uri >>= fn)
      (fun e ->
        Log.debug (fun l ->
            l "request to %a failed: %a" Uri.pp_hum uri Fmt.exn e);
        Lwt.fail e)

  let call meth t ctx ?body path parse =
    map_call meth t ctx ~keep_alive:false ?body path (map_string_response parse)

  let call_stream meth t ctx ?body path parse =
    map_call meth t ctx ~keep_alive:true ?body path (map_stream_response parse)
end

module RO (Client : Cohttp_lwt.S.Client) (K : Irmin.Type.S) (V : Irmin.Type.S) :
  S.Read_only.S
    with type ctx = Client.ctx
     and type key = K.t
     and type value = V.t = struct
  type ctx = Client.ctx

  module HTTP = Helper (Client)

  type 'a t = {
    uri : Uri.t;
    item : string;
    items : string;
    ctx : Client.ctx option;
  }

  let uri t = t.uri
  let item t = t.item
  let items t = t.items
  let close _ = Lwt.return ()

  type key = K.t
  type value = V.t

  let key_str = Irmin.Type.to_string K.t
  let val_of_str = Irmin.Type.of_string V.t

  let find t key =
    HTTP.map_call `GET t.uri t.ctx ~keep_alive:false [ t.item; key_str key ]
      (fun ((r, _) as x) ->
        if Cohttp.Response.status r = `Not_found then Lwt.return_none
        else HTTP.map_string_response val_of_str x >|= Option.some)

  let mem t key =
    HTTP.map_call `GET t.uri t.ctx ~keep_alive:false [ t.item; key_str key ]
      (fun (r, _) ->
        if Cohttp.Response.status r = `Not_found then Lwt.return_false
        else Lwt.return_true)

  let v ?ctx uri item items = Lwt.return { uri; item; items; ctx }
end

module CA (Client : Cohttp_lwt.S.Client) (K : Irmin.Hash.S) (V : Irmin.Type.S) =
struct
  include RO (Client) (K) (V)

  let add t value =
    let body = Irmin.Type.to_string V.t value in
    HTTP.call `POST t.uri t.ctx [ t.items ] ~body (Irmin.Type.of_string K.t)

  let unsafe_add t key value =
    let body = Irmin.Type.to_string V.t value in
    HTTP.call `POST t.uri t.ctx
      [ "unsafe"; t.items; key_str key ]
      ~body
      Irmin.Type.(of_string unit)

  let cast t = (t :> read_write t)

  let batch t f =
    (* TODO:cache the writes locally and send everything in one batch *)
    f (cast t)

  let close _ = Lwt.return_unit

  let clear t =
    HTTP.call `POST t.uri t.ctx [ "clear"; t.items ] Irmin.Type.(of_string unit)
end

module RW : S.Atomic_write.Maker =
functor
  (Client : Cohttp_lwt.S.Client)
  (K : Irmin.Type.S)
  (V : Irmin.Type.S)
  ->
  struct
    module RO = RO (Client) (K) (V)
    module HTTP = RO.HTTP
    module W = Irmin.Private.Watch.Make (K) (V)

    type key = RO.key
    type value = RO.value
    type watch = W.watch
    type ctx = Client.ctx

    (* cache the stream connections to the server: we open only one
       connection per stream kind. *)
    type cache = { mutable stop : unit -> unit }

    let empty_cache () = { stop = (fun () -> ()) }

    type t = { t : read RO.t; w : W.t; keys : cache; glob : cache }

    let get t = HTTP.call `GET (RO.uri t.t) t.t.ctx
    let put t = HTTP.call `PUT (RO.uri t.t) t.t.ctx
    let get_stream t = HTTP.call_stream `GET (RO.uri t.t) t.t.ctx
    let post_stream t = HTTP.call_stream `POST (RO.uri t.t) t.t.ctx

    let v ?ctx uri item items =
      let* t = RO.v ?ctx uri item items in
      let w = W.v () in
      let keys = empty_cache () in
      let glob = empty_cache () in
      Lwt.return { t; w; keys; glob }

    let find t = RO.find t.t
    let mem t = RO.mem t.t
    let key_str = Irmin.Type.to_string K.t
    let list t = get t [ RO.items t.t ] (of_json T.(list K.t))

    let set t key value =
      let value = { v = Some value; set = None; test = None } in
      let body = to_json (set_t V.t) value in
      put t [ RO.item t.t; key_str key ] ~body (of_json status_t) >>= function
      | { status = "ok" } -> Lwt.return_unit
      | e -> Lwt.fail_with e.status

    let test_and_set t key ~test ~set =
      let value = { v = None; set; test } in
      let body = to_json (set_t V.t) value in
      put t [ RO.item t.t; key_str key ] ~body (of_json status_t) >>= function
      | { status = "true" } -> Lwt.return_true
      | { status = "false" } -> Lwt.return_false
      | e -> Lwt.fail_with e.status

    let pp_key = Irmin.Type.pp K.t

    let remove t key =
      HTTP.map_call `DELETE (RO.uri t.t) t.t.ctx ~keep_alive:false
        [ RO.item t.t; key_str key ] (fun (r, b) ->
          match Cohttp.Response.status r with
          | `Not_found | `OK -> Lwt.return_unit
          | _ ->
              let* b = Cohttp_lwt.Body.to_string b in
              Fmt.kstrf Lwt.fail_with "cannot remove %a: %s" pp_key key b)

    let nb_keys t = fst (W.stats t.w)
    let nb_glob t = snd (W.stats t.w)

    (* run [t] and returns an handler to stop the task. *)
    let stoppable t =
      let s, u = Lwt.task () in
      Lwt.async (fun () -> Lwt.pick [ s; t () ]);
      function () -> Lwt.wakeup u ()

    let watch_key t key ?init f =
      let key_str = Irmin.Type.to_string K.t key in
      let init_stream () =
        if nb_keys t <> 0 then Lwt.return_unit
        else
          let* s =
            match init with
            | None -> get_stream t [ "watch"; key_str ] (event_t K.t V.t)
            | Some init ->
                let body = to_json V.t init in
                post_stream t [ "watch"; key_str ] ~body (event_t K.t V.t)
          in
          let stop () =
            Lwt_stream.iter_s
              (fun { diff; _ } ->
                let diff =
                  match diff with
                  | `Removed _ -> None
                  | `Added v | `Updated (_, v) -> Some v
                in
                W.notify t.w key diff)
              s
          in
          t.keys.stop <- stoppable stop;
          Lwt.return_unit
      in
      let* () = init_stream () in
      W.watch_key t.w key ?init f

    let watch t ?init f =
      let init_stream () =
        if nb_glob t <> 0 then Lwt.return_unit
        else
          let* s =
            match init with
            | None -> get_stream t [ "watches" ] (event_t K.t V.t)
            | Some init ->
                let init =
                  List.map (fun (branch, commit) -> { branch; commit }) init
                in
                let body = to_json T.(list (init_t K.t V.t)) init in
                post_stream t [ "watches" ] ~body (event_t K.t V.t)
          in
          let stop () =
            Lwt_stream.iter_s
              (fun ev ->
                let diff =
                  match ev.diff with
                  | `Removed _ -> None
                  | `Added v | `Updated (_, v) -> Some v
                in
                let k = ev.branch in
                Log.debug (fun l ->
                    let pp_opt =
                      Fmt.option ~none:(Fmt.any "<none>") (Irmin.Type.pp V.t)
                    in
                    l "notify %a: %a" pp_key k pp_opt diff);
                W.notify t.w k diff)
              s
          in
          t.glob.stop <- stoppable stop;
          Lwt.return_unit
      in
      let* () = init_stream () in
      W.watch t.w ?init f

    let stop x =
      let () = try x.stop () with _e -> () in
      x.stop <- (fun () -> ())

    let unwatch t id =
      W.unwatch t.w id >>= fun () ->
      if nb_keys t = 0 then stop t.keys;
      if nb_glob t = 0 then stop t.glob;
      Lwt.return_unit

    let close _ = Lwt.return_unit

    let clear t =
      HTTP.call `POST t.t.uri t.t.ctx [ "clear"; t.t.items ]
        Irmin.Type.(of_string unit)
  end

module type HTTP_CLIENT = sig
  include Cohttp_lwt.S.Client

  val ctx : unit -> ctx option
end

module Client (Client : HTTP_CLIENT) (S : Irmin.S) = struct
  module X = struct
    module Hash = S.Hash
    module Schema = S.Schema

    module Contents = struct
      module X = struct
        module Key = S.Hash
        module Val = S.Contents
        module CA = CA (Client) (Key) (Val)
        include Closeable.Content_addressable (CA)
      end

      include Irmin.Contents.Store (X) (X.Key) (X.Val)

      let v ?ctx config = X.v ?ctx config "blob" "blobs"
    end

    module Node = struct
      module Val = S.Private.Node.Val
      module Key = Irmin.Hash.Typed (S.Hash) (Val)
      module CA = CA (Client) (S.Hash) (Val)
      include CA
      module Contents = Contents
      module Metadata = S.Metadata
      module Path = S.Key

      let merge t =
        let f ~(old : Key.t option Irmin.Merge.promise) left right =
          let* old =
            old () >|= function
            | Ok (Some old) -> old
            | Ok None -> None
            | Error _ -> None
          in
          let body =
            Irmin.Type.(to_string (merge_t (option Key.t))) { old; left; right }
          in
          let result = merge_result_t Key.t in
          HTTP.call `POST t.uri t.ctx [ t.items; "merge" ] ~body
            (Irmin.Type.of_string result)
        in
        Irmin.Merge.(v Irmin.Type.(option Key.t)) f

      let v ?ctx config = v ?ctx config "tree" "trees"
    end

    module Commit = struct
      module X = struct
        module Key = S.Hash
        module Val = S.Private.Commit.Val
        module CA = CA (Client) (Key) (Val)
        include Closeable.Content_addressable (CA)
      end

      include Irmin.Commit.Store (S.Info) (Node) (X) (X.Key) (X.Val)

      let v ?ctx config = X.v ?ctx config "commit" "commits"
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Remote = Irmin.Private.Remote.None (Hash) (S.Branch)

    module Branch = struct
      module Key = S.Branch
      module Val = S.Hash
      include Closeable.Atomic_write (RW (Client) (Key) (Val))

      let v ?ctx config = v ?ctx config "branch" "branches"
    end

    module Repo = struct
      type t = {
        config : Irmin.config;
        contents : read Contents.t;
        node : read Node.t;
        commit : read Commit.t;
        branch : Branch.t;
      }

      let branch_t t = t.branch
      let commit_t t = t.commit
      let node_t t = t.node
      let contents_t t = t.contents

      let batch t f =
        Contents.X.batch t.contents @@ fun contents_t ->
        Node.batch t.node @@ fun node_t ->
        Commit.X.batch (snd t.commit) @@ fun commit_t ->
        let commit_t = (node_t, commit_t) in
        f contents_t node_t commit_t

      let v config =
        let uri = get_uri config in
        let ctx = Client.ctx () in
        let* contents = Contents.v ?ctx uri in
        let* node = Node.v ?ctx uri in
        let* commit = Commit.v ?ctx uri in
        let+ branch = Branch.v ?ctx uri in
        let commit = (node, commit) in
        { contents; node; commit; branch; config }

      let close t =
        let* () = Contents.X.close t.contents in
        let* () = Branch.close t.branch in
        Commit.X.close (snd t.commit)
    end
  end

  include Irmin.Of_private (X)
end

module type SERVER = Irmin_http_server.S

module Server = Irmin_http_server.Make
