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

module type Jsonable = sig
  include Identifiable.S
  val to_json: t -> Ezjsonm.t
  val of_json: Ezjsonm.t -> t
end

module Make (Client: Cohttp_lwt.Client) = struct

  module L = Log.Make(struct let section = "CRUD" end)

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
    L.debugf "response: body=%s" b;
    let j = Ezjsonm.from_string b in
    try return (fn (result_of_json j))
    with Error e -> fail (Error e)

  let map_stream_response fn (r,b) =
    let stream = Cohttp_lwt_body.to_stream b in
    let stream = Ezjsonm_lwt.from_stream stream in
    let stream = Lwt_stream.map result_of_json stream in
    Lwt_stream.map fn stream

  let map_get t path fn =
    L.debugf "get %s" (Uri.to_string (uri t path));
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
    L.debugf "delete %s" (Uri.to_string (uri t path));
    Cohttp_lwt_unix.Client.delete (uri t path) >>=
    map_string_response fn

  let post t path body fn =
    let body =
      let params = `O [ "params", body ] in
      Ezjsonm.to_string params in
    L.debugf "post %s %s" (Uri.to_string (uri t path)) body;
    let body = Cohttp_lwt_body.of_string body in
    Cohttp_lwt_unix.Client.post ~body (uri t path) >>=
    map_string_response fn

  module type U = sig
    val uri: Uri.t
  end

  module RO (U: U) (K: IrminKey.S) (V: Jsonable) = struct

    module L = Log.Make(struct let section = "CRUD" ^ Uri.path U.uri end)

    type t = Uri.t

    type key = K.t

    type value = V.t

    let some fn x =
      Some (fn x)

    let create () =
      return U.uri

    let read t key =
      L.debugf "read %s" (K.to_string key);
      catch
        (fun () -> get t ["read"; K.to_string key] (some V.of_json))
        (fun _  -> return_none)

    let read_exn t key =
      L.debugf "read_exn %s" (K.to_string key);
      get t ["read"; K.to_string key] V.of_json

    let mem t key =
      L.debugf "mem %s" (K.to_string key);
      get t ["mem"; K.to_string key] Ezjsonm.get_bool


    let list t key =
      L.debugf "list %s" (K.to_string key);
      get t ["list"; K.to_string key] (Ezjsonm.get_list K.of_json)

    let dump t =
      L.debugf "dump";
      get t ["dump"] (Ezjsonm.get_list (Ezjsonm.get_pair K.of_json V.of_json))

  end

  module AO (U: U) (K: IrminKey.S) (V: Jsonable) = struct

    include RO(U)(K)(V)

    let add t value =
      L.debugf "add %S"(V.to_string value);
      post t ["add"] (V.to_json value) K.of_json

  end

  module RW (U: U) (K: IrminKey.S) (V: Jsonable) = struct

    include RO(U)(K)(V)

    let update t key value =
      L.debugf "update %s %S" (K.to_string key) (V.to_string value);
      post t ["update"; K.to_string key] (V.to_json value) Ezjsonm.get_unit

    let remove t key =
      L.debugf "remove %s" (K.to_string key);
      delete t ["remove"; K.to_string key] Ezjsonm.get_unit

    let watch t path =
      L.debugf "watch";
      get_stream t ["watch"; K.to_string path] V.of_json

  end

  module S (U: U)
      (K: IrminKey.S)
      (V: Jsonable)
      (S: IrminKey.S)
      (B: IrminKey.S)
      (D: Jsonable)
  = struct

    include RW(U)(K)(V)

    type snapshot = S.t

    type dump = D.t

    type branch = B.t

    let snapshot t =
      L.debugf "snapshot";
      get t ["snapshot"] S.of_json

    let revert t rev =
      L.debugf "revert";
      get t ["revert"; S.to_string rev] Ezjsonm.get_unit

    let merge_snapshot t s1 s2 =
      L.debugf "merge_snapshot";
      get t ["merge_snapshot"; S.to_string s1; S.to_string s2] S.of_json

    let watch t path =
      L.debugf "watch";
      get_stream t ["watch"; K.to_string path] (Ezjsonm.get_pair K.of_json S.of_json)

    let export t revs =
      L.debugf "export %s" (IrminMisc.pretty_list S.to_string revs);
      get t ("export" :: List.map ~f:S.to_string revs) D.of_json

    let import t branch dump =
      L.debugf "dump";
      post t ["import"; B.to_string branch] (D.to_json dump) Ezjsonm.get_unit

  end

  module Make (K: IrminKey.S) (B: IrminContents.S) (R: IrminReference.S) = struct

    module N = IrminNode.S(K)
    module C = IrminCommit.S(K)

    let create u =
      let module Contents = AO(struct
          let uri = uri u ["contents"]
        end)(K)(B) in
      let module Node = AO(struct
          let uri = uri u ["node"]
        end)(K)(N) in
      let module Commit = AO(struct
          let uri = uri u ["commit"]
        end)(K)(C) in
      let module Reference = RW(struct
          let uri = uri u ["ref"]
        end)(R)(K) in
      let module Store = S(struct
          let uri = u
        end) in
      let module Internal = IrminValue.Mux(K)(B)(Contents)(Node)(Commit) in
      let module Reference = IrminReference.Make(R)(K)(Reference) in
      let module S = Irmin.Make(K)(B)(R)(Internal)(Reference) in
      (module S: Irmin.S with type Internal.key = K.t
                          and type value = B.t
                          and type Reference.key = R.t)

    let cast (module M: Irmin.S with type Internal.key = K.t
                                 and type value = B.t
                                 and type Reference.key = R.t) =

      (module M: Irmin.S)

  end

end

module type S = sig
  module type U = sig
    val uri: Uri.t
  end
  module RO (U: U) (K: IrminKey.S) (V: Jsonable):
    IrminStore.RO with type key = K.t and type value = V.t
  module AO (U: U) (K: IrminKey.S) (V: Jsonable):
    IrminStore.AO with type key = K.t and type value = V.t
  module RW (U: U) (K: IrminKey.S) (V: Jsonable):
    IrminStore.RW with type key = K.t and type value = V.t
  module Make
    (K: IrminKey.S)
    (C: IrminContents.S)
    (R: IrminReference.S):
  sig
    val create: Uri.t ->  (K.t, C.t, R.t) Irmin.t
    val cast:  (K.t, C.t, R.t) Irmin.t -> (module Irmin.S)
  end
end
