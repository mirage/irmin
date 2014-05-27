(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Core_kernel.Std

module type Config = sig

  val uri: Uri.t
  (** The server URI. *)

end

module XLog = Log

module XMake (Client: Cohttp_lwt.Client) = struct

  module Log = XLog.Make(struct let section = "CRUD" end)

  exception Error of string

  let uri t path = match Uri.path t :: path with
    | []   -> t
    | path -> Uri.with_path t (IrminPath.to_raw path)

  type ('a, 'b) response =
    (Ezjsonm.t -> 'a) -> (Cohttp.Response.t * Cohttp_lwt_body.t) -> 'b

  let result_of_json json =
    let error =
      try Some (Ezjsonm.find json ["error"])
      with Not_found -> None in
    let result =
      try Some (Ezjsonm.find json ["result"])
      with Not_found -> None in
    match error, result with
    | None  , None   -> raise (Error "result_of_json")
    | Some e, None   -> raise (Error (IrminMisc.json_decode_exn e))
    | None  , Some r -> r
    | Some _, Some _ -> raise (Error "result_of_json")

  let map_string_response fn (r,b) =
    Cohttp_lwt_body.to_string b >>= fun b ->
    Log.debugf "response: body=%s" b;
    let j = Ezjsonm.from_string b in
    try return (fn (result_of_json j))
    with Error e -> fail (Error e)

  let map_stream_response fn (r,b) =
    let stream = Cohttp_lwt_body.to_stream b in
    let stream = Ezjsonm_lwt.from_stream stream in
    let stream = Lwt_stream.map result_of_json stream in
    Lwt_stream.map fn stream

  let map_get t path fn =
    Log.debugf "get %s" (Uri.to_string (uri t path));
    Client.get (uri t path) >>=
    fn

  let get t path fn =
    map_get t path (map_string_response fn)

  let get_stream t path fn  =
    let (stream: 'a Lwt_stream.t option ref) = ref None in
    let rec get () =
      match !stream with
      | Some s -> Lwt_stream.get s
      | None   ->
        map_get t path (fun b ->
            let s = map_stream_response fn b in
            stream := Some s;
            return_unit) >>= fun () ->
        get () in
    Lwt_stream.from get

  let delete t path fn =
    Log.debugf "delete %s" (Uri.to_string (uri t path));
    Cohttp_lwt_unix.Client.delete (uri t path) >>=
    map_string_response fn

  let post t path body fn =
    let body =
      let params = `O [ "params", body ] in
      Ezjsonm.to_string params in
    Log.debugf "post %s %s" (Uri.to_string (uri t path)) body;
    let body = Cohttp_lwt_body.of_string body in
    Cohttp_lwt_unix.Client.post ~body (uri t path) >>=
    map_string_response fn

  module RO (U: Config) (K: IrminKey.S) (V: IrminIdent.S) = struct

    module Log = XLog.Make(struct let section = "CRUD" ^ Uri.path U.uri end)

    type t = Uri.t

    type key = K.t

    type value = V.t

    let some fn x =
      Some (fn x)

    let create () =
      return U.uri

    let read t key =
      Log.debugf "read %s" (K.to_string key);
      catch
        (fun () -> get t ["read"; K.to_string key] (some V.of_json))
        (fun _  -> return_none)

    let read_exn t key =
      Log.debugf "read_exn %s" (K.to_string key);
      get t ["read"; K.to_string key] V.of_json

    let mem t key =
      Log.debugf "mem %s" (K.to_string key);
      get t ["mem"; K.to_string key] Ezjsonm.get_bool


    let list t keys =
      Log.debugf "list %s" (IrminMisc.pretty_list K.to_string keys);
      get t ("list" :: List.map ~f:K.to_string keys) (Ezjsonm.get_list K.of_json)

    let dump t =
      Log.debugf "dump";
      get t ["dump"] (Ezjsonm.get_list (Ezjsonm.get_pair K.of_json V.of_json))

  end

  module AO (U: Config) (K: IrminKey.S) (V: IrminIdent.S) = struct

    include RO(U)(K)(V)

    let add t value =
      Log.debugf "add";
      post t ["add"] (V.to_json value) K.of_json

  end

  module RW (U: Config) (K: IrminKey.S) (V: IrminIdent.S) = struct

    include RO(U)(K)(V)

    let update t key value =
      Log.debugf "update %s" (K.to_string key);
      post t ["update"; K.to_string key] (V.to_json value) Ezjsonm.get_unit

    let remove t key =
      Log.debugf "remove %s" (K.to_string key);
      delete t ["remove"; K.to_string key] Ezjsonm.get_unit

    let watch t path =
      Log.debugf "watch";
      get_stream t ["watch"; K.to_string path] V.of_json

  end

(*
  module BC (U: Config) (K: Key) (C: IrminContents.S) (T: IrminTag.S) = struct

    module RW = RW(U)(K)(C)

    type t = Uri.t * T.t

    type branch = T.t

    let create ?branch t =
      let branch = match branch with
        | None   -> T.master
        | Some b -> b in
      U.uri, branch

    let branch (_, b) = b

    let with_branch (t, _) b = (t, b)

    let json_of_origin =
      Ezjsonm.option IrminOrigin.to_json

    let update t ?origin key value =
      Log.debugf "update %s" (K.to_string key);
      post t ["update"; K.to_string key]
        (Ezjson.pair json_of_origin V.to_json (origin, value))
        Ezjsonm.get_unit

    let remove t ?origin key =
      Log.debugf "remove %s" (K.to_string key);
      post t ["remove"; K.to_string key] (json_of_origin origin) Ezjsonm.get_unit

    let clone t branch =
      Log.debugf "clone %s" (T.to_string branch);
      post t ["clone"; T.to_string branch ] >>= fun () ->
      with_branch t branch

    let force_clone t branch =
      Log.debugf "force_clone %s" (T.to_string branch);
      post t ["force-clone"; T.to_string branch ] >>= fun () ->
      with_branch t branch

    let merge t ?origin branch =
      Log.debugf "merge";
      post t ["merge"; T.to_string branch]
        (json_of_origin origin) (result_of_json Ezjsonm.get_unit)

    let merge_exn t ?origin branch =
      Log.debugf "merge";
      post t ["merge"; T.to_string branch]
        (json_of_origin origin) Ezjsonm.get_unit

  end
*)

  module Make (U: Config) (K: IrminKey.S) (B: IrminContents.S) (T: IrminTag.S) = struct

    module N = IrminNode.S(K)
    module C = IrminCommit.S(K)

    module XContents = AO(struct
        let uri = uri U.uri ["contents"]
      end)(K)(B)

    module XNode = AO(struct
        let uri = uri U.uri ["node"]
      end)(K)(N)

    module XCommit = AO(struct
        let uri = uri U.uri ["commit"]
      end)(K)(C)

    module XTag = RW(struct
        let uri = uri U.uri ["tag"]
      end)(T)(K)

    module XXBlock = IrminBlock.Mux(K)(B)(XContents)(XNode)(XCommit)
    module XXTag = IrminTag.Make(T)(K)(XTag)

    include Irmin.Make(XXBlock)(XXTag)

  end

end

module Make (C: Cohttp_lwt.Client) (U: Config) = struct
  module M = XMake(C)
  module RO = M.RO(U)
  module AO = M.AO(U)
  module RW = M.RW(U)
  module Make = M.Make(U)
end
