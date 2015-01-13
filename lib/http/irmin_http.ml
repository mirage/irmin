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
open Irmin.Merge.OP

module Log = Log.Make(struct let section = "HTTP" end)

(* ~uri *)
let uri =
  Irmin.Private.Conf.key
    ~docv:"URI"
    ~doc:"Location of the remote store."
    "uri" Irmin.Private.Conf.(some uri) None

let config x =
  Irmin.Private.Conf.singleton uri (Some x)

module type Config = sig val suffix: string option end

module Helper (Client: Cohttp_lwt.Client) = struct

  exception Error of string

  let uri_append t path = match Uri.path t :: path with
    | []   -> t
    | path ->
      let buf = Buffer.create 10 in
      List.iter (function
          | "" -> ()
          | s  ->
            if s.[0] <> '/' then Buffer.add_char buf '/';
            Buffer.add_string buf s;
        ) path;
      let path = Buffer.contents buf in
      Uri.with_path t path

  let result_of_json json =
    let error =
      try Some (Ezjsonm.find json ["error"])
      with Not_found -> None in
    let result =
      try Some (Ezjsonm.find json ["result"])
      with Not_found -> None in
    match error, result with
    | None  , None   -> raise (Error "result_of_json")
    | Some e, None   -> raise (Error (Ezjsonm.decode_string_exn e))
    | None  , Some r -> r
    | Some _, Some _ -> raise (Error "result_of_json")

  let map_string_response (type t) (module M: Tc.S0 with type t = t) (_, b) =
    Cohttp_lwt_body.to_string b >>= fun b ->
    Log.debug "got response: %s" b;
    let j = Ezjsonm.from_string b in
    try
      Ezjsonm.value j
      |> result_of_json
      |> M.of_json
      |> return
    with Error e ->
      fail (Error e)

  let map_stream_response (type t) (module M: Tc.S0 with type t = t) (_, b) =
    let stream = Cohttp_lwt_body.to_stream b in
    let stream = Ezjsonm_lwt.from_stream stream in
    let stream = Lwt_stream.map result_of_json stream in
    Lwt_stream.map (fun j ->
        Log.debug "stream get %s" Ezjsonm.(to_string (wrap j));
        M.of_json j
      ) stream

  let map_get t path fn =
    let uri = uri_append t path in
    Log.debug "get %s" (Uri.path uri);
    Client.get uri >>= fun r ->
    fn r

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
    let uri = uri_append t path in
    Log.debug "delete %s" (Uri.path uri);
    Client.delete uri >>=
    map_string_response fn

  let post t path body fn =
    let body =
      let params = `O [ "params", body ] in
      Ezjsonm.to_string params in
    let uri = uri_append t path in
    let short_body =
      if String.length body > 80 then String.sub body 0 80 ^ ".." else body
    in
    Log.debug "post %s %s" (Uri.path uri) short_body;
    let body = Cohttp_lwt_body.of_string body in
    Client.post ~body uri >>=
    map_string_response fn

end

module Low (Client: Cohttp_lwt.Client)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
    (H: Irmin.Hash.S) =
struct

  include Helper (Client)

  module RO (C: Config) (K: Irmin.Hum.S) (V: Tc.S0) = struct

    type t = {
      mutable uri: Uri.t;
      task: Irmin.task;
    }

    let task t = t.task

    type key = K.t
    type value = V.t

    let create_aux config task =
      let uri = match Irmin.Private.Conf.get config uri with
        | None   -> failwith "Irmin_http.create: No URI specified"
        | Some u -> u
      in
      let uri = match C.suffix with
        | None   -> uri
        | Some p -> uri_append uri [p]
      in
      { uri; task = task }

    let create config task =
      return (fun a -> create_aux config (task a))

    let read { uri; _ } key =
      get uri ["read"; K.to_hum key] (module Tc.Option(V))

    let read_exn t key =
      read t key >>= function
      | None   -> fail Not_found
      | Some v -> return v

    let mem { uri; _ } key =
      get uri ["mem"; K.to_hum key] Tc.bool

    let iter { uri; _ } fn =
      Lwt_stream.iter_p fn (get_stream uri ["iter"] (module K))

  end

  module AO (C: Config) (K: Irmin.Hash.S) (V: Tc.S0) = struct

    include RO (C)(K)(V)

    let add { uri; _ } value =
      post uri ["add"] (V.to_json value) (module K)

  end

  module RW (C: Config) (K: Irmin.Hum.S) (V: Tc.S0) = struct

    include RO (C)(K)(V)

    let update { uri; _ } key value =
      post uri ["update"; K.to_hum key] (V.to_json value) Tc.unit

    let remove { uri; _ } key =
      delete uri ["remove"; K.to_hum key] Tc.unit

    let watch { uri; _ } path =
      get_stream uri ["watch"; K.to_hum path] (module Tc.Option(V))

  end

  module X = struct
    module Contents = Irmin.Contents.Make(struct
        module Key = H
        module Val = C
        include AO(struct let suffix = Some "contents" end)(H)(C)
      end)
    module Node = struct
      module Key = H
      module Path = C.Path
      module Val = Irmin.Private.Node.Make(H)(H)(C.Path)
      include AO(struct let suffix = Some "node" end)(Key)(Val)
    end
    module Commit = struct
      module Key = H
      module Val = Irmin.Private.Commit.Make(H)(H)
      include AO(struct let suffix = Some "commit" end)(Key)(Val)
    end
    module Tag = struct
      module Key = T
      module Val = H
      include RW(struct let suffix = Some "tag" end)(Key)(Val)
    end
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = Irmin.Private.Sync.None(H)(T)
  end

  include Irmin.Make_ext(X)
end


module AO (Client: Cohttp_lwt.Client) (K: Irmin.Hash.S) (V: Tc.S0) = struct
  module V = struct
    include V
    let merge _path ~old:_ _ _ = failwith "Irmin_git.AO.merge"
    module Path = Irmin.Path.String_list
  end
  module M = Low (Client)(V)(Irmin.Tag.String)(K)
  include M.X.Contents
end

module RW (Client: Cohttp_lwt.Client) (K: Irmin.Hum.S) (V: Irmin.Hash.S) = struct
  module K = struct
    include K
    let master = K.of_hum "master"
  end
  module M = Low (Client)(Irmin.Contents.String)(K)(V)
  include M.X.Tag
end

module Make (Client: Cohttp_lwt.Client)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
    (H: Irmin.Hash.S) =
struct

  module T = struct
    include T
    let to_hum t = Uri.pct_encode (to_hum t)
    let of_hum t = of_hum (Uri.pct_decode t)
  end

  module P = struct

    include C.Path

    let to_hum t =
      String.concat "/" (C.Path.map t (fun x -> Uri.pct_encode (Step.to_hum x)))

    let of_hum t =
      List.filter ((<>)"") (Stringext.split t ~on:'/')
      |> List.map (fun x -> Step.of_hum (Uri.pct_decode x))
      |> C.Path.create

  end

  include Helper (Client)

  (* Implementing a high-level HTTP BC backend is a bit tricky as we
     need to keep track of some hidden state which is not directly
     exposed by the interface. This is the case when we are in
     `detached` mode, and an high-level update does not return the new
     head value.

     We solve this by tapping updating the HTTP API to return more
     information than the OCaml API dictates. in lower-level
     bindings. *)

  (* The high-level bindings: every high-level operation is simply
     forwarded to the HTTP server. *much* more efficient than using
     [L]. *)
  module L = Low(Client)(C)(T)(H)
  module LP = L.Private
  module S = L.RW(struct let suffix = None end)(P)(C)

  (* [t.s.uri] always point to the right location:
       - `$uri/` if branch = `Tag T.master
       - `$uri/tree/$tag` if branch = `Tag tag
       - `$uri/tree/$key if key = `Key key *)
  type t = {
    mutable branch: [`Tag of T.t | `Head of H.t ];
    mutable h: S.t;
    config: Irmin.config;
    contents_t: LP.Contents.t;
    node_t: LP.Node.t;
    commit_t: LP.Commit.t;
    tag_t: LP.Tag.t;
    read_node: L.key -> LP.Node.key option Lwt.t;
    mem_node: L.key -> bool Lwt.t;
    update_node: L.key -> LP.Node.key -> unit Lwt.t;
  }

  let uri t =
    let base = t.h.S.uri in
    match t.branch with
    | `Tag tag ->
      if T.equal tag T.master then base
      else uri_append base ["tree"; T.to_hum tag]
    | `Head h ->
      uri_append base ["tree"; H.to_hum h]

  let task t = S.task t.h
  let branch t = t.branch
  let set_tag t tag = t.branch <- `Tag tag
  let set_head t head = t.branch <- `Head head

  type key = S.key
  type value = S.value
  type head = L.head
  type tag = L.tag

  let create config task =
    S.create config task >>= fun h ->
    L.create config task >>= fun l ->
    let fn a =
      let h = h a in
      let l = l a in
      let branch = `Tag T.master in
      let contents_t = LP.contents_t l in
      let node_t = LP.node_t l in
      let commit_t = LP.commit_t l in
      let tag_t = LP.tag_t l in
      let read_node = LP.read_node l in
      let mem_node = LP.mem_node l in
      let update_node = LP.update_node l in
      { branch; h; contents_t; node_t; commit_t; tag_t;
        read_node; mem_node; update_node; config; }
    in
    return fn

  let of_tag config task tag =
    create config task >>= fun t ->
    return (fun a ->
        let t = t a in
        set_tag t tag;
        t
      )

  let of_head config task head =
    S.create config task >>= fun h ->
    L.of_head config task head >>= fun l ->
    let fn a =
      let h = h a in
      let l = l a in
      let branch = `Tag T.master in
      let contents_t = LP.contents_t l in
      let node_t = LP.node_t l in
      let commit_t = LP.commit_t l in
      let tag_t = LP.tag_t l in
      let read_node = LP.read_node l in
      let mem_node = LP.mem_node l in
      let update_node = LP.update_node l in
      { branch; h; contents_t; node_t; commit_t; tag_t;
        read_node; mem_node; update_node; config; }
    in
    return fn

  let read t key =
    get (uri t) ["read"; P.to_hum key] (module Tc.Option(C))

  let read_exn t key =
    read t key >>= function
    | None   -> fail Not_found
    | Some v -> return v

  let mem t key =
    get (uri t) ["mem"; P.to_hum key] Tc.bool

  let iter t fn =
    Lwt_stream.iter_p fn (get_stream (uri t) ["iter"] (module P))

  let watch t path =
    get_stream (uri t) ["watch"; P.to_hum path] (module Tc.Option(C))

  let update t key value =
    post (uri t) ["update"; P.to_hum key] (C.to_json value) (module H)
    >>= fun h ->
    let () = match t.branch with
      | `Head _ -> set_head t h
      | `Tag  _ -> ()
    in
    return_unit

  let remove t  key =
    delete (uri t) ["remove"; P.to_hum key] (module H) >>= fun h ->
    match t.branch with
    | `Head _ -> set_head t h; return_unit
    | `Tag _  -> return_unit

  let tag t = match t.branch with
    | `Head _ -> None
    | `Tag t  -> Some t

  let tag_exn t = match tag t with
    | None   -> raise Not_found
    | Some t -> t

  let tags t =
    get (uri t) ["tags"] (module Tc.List(T))

  let head t = match t.branch with
    | `Head h -> return (Some h)
    | `Tag _  -> get (uri t) ["head"] (module Tc.Option(H))

  let head_exn t =
    head t >>= function
    | None   -> fail Not_found
    | Some h -> return h

  let rename_tag t tag =
    get (uri t) ["rename-tag"; T.to_hum tag] Tc.string >>= function
    | "ok" -> set_tag t tag; return `Ok
    | _    -> return `Duplicated_tag

  let update_tag t tag =
    get (uri t) ["update-tag"; T.to_hum tag] Tc.unit >>= fun () ->
    set_tag t tag;
    return_unit

  let switch t tag =
    match t.branch with
    | `Head _ -> set_tag t tag; return_unit
    | `Tag _  ->
      get (uri t) ["switch"; T.to_hum tag] Tc.unit >>= fun () ->
      set_tag t tag;
      return_unit

  let heads t =
    get (uri t) ["heads"] (module Tc.List(H))

  let detach t =
    match t.branch with
    | `Head _ -> return_unit
    | `Tag _  ->
      head t >>= function
      | None   -> return_unit
      | Some h -> set_head t h; return_unit

  let update_head t head =
    match t.branch with
    | `Head _ -> set_head t head; return_unit
    | `Tag _  -> get (uri t) ["update-head"; H.to_hum head] Tc.unit

  module M = Tc.App1 (Irmin.Merge.Result) (H)

  let merge_head t head =
    get (uri t) ["merge-head"; H.to_hum head] (module M) >>| fun h ->
    match t.branch with
    | `Head _ -> set_head t h; ok ()
    | `Tag _  -> ok ()

  let merge_head_exn t head =
    merge_head t head >>=
    Irmin.Merge.exn

  module W = Tc.Pair (P)(H)

  let watch_head t key =
    match t.branch with
    | `Head _ -> Lwt_stream.of_list []
    | `Tag _  ->
      Irmin.Private.Watch.lwt_stream_lift (
        let s = get_stream (uri t) ["watch-head"; P.to_hum key] (module W) in
        return s
      )

  let clone task t tag =
    get (uri t) ["clone"; T.to_hum tag] Tc.string >>= function
    | "ok" ->
      of_tag t.config task tag >>= fun t ->
      return (`Ok t)
    | _    -> return `Duplicated_tag

  let clone_force task t tag =
    get (uri t) ["clone-force"; T.to_hum tag] Tc.unit >>= fun () ->
    of_tag t.config task tag

  let merge_tag t tag =
    get (uri t) ["merge-tag"; T.to_hum tag] (module M) >>| fun h ->
    match t.branch with
    | `Head _ -> set_head t h; ok ()
    | `Tag _  -> ok ()

  let merge_tag_exn t tag = merge_tag t tag >>= Irmin.Merge.exn

  let merge a t ~into =
    let t = t a and into = into a in
    match branch t with
    | `Tag tag -> merge_tag into tag
    | `Head h  -> merge_head into h

  let merge_exn a t ~into = merge a t ~into >>= Irmin.Merge.exn

  module E = Tc.Pair
      (Tc.Pair (Tc.Option(Tc.Bool)) (Tc.Option(Tc.Int)))
      (Tc.Pair (Tc.List(H)) (Tc.List(H)))

  type slice = L.slice

  module Slice = L.Private.Slice

  let export ?full ?depth ?(min=[]) ?(max=[]) t =
    post (uri t) ["export"] (E.to_json ((full, depth), (min, max)))
      (module L.Private.Slice)

  module I = Tc.List(T)

  let import t slice =
    post (uri t) ["import"] (Slice.to_json slice) Tc.unit

  let remove_rec t dir =
    get (uri t) ["remove-rec"; P.to_hum dir] (module H) >>= fun h ->
    match t.branch with
    | `Head _ -> set_head t h; return_unit
    | `Tag _  -> return_unit

  let list t dir =
    get (uri t) ["list"; P.to_hum dir] (module Tc.List(P))

  module Key = P
  module Val = C
  module Tag = T
  module Head = H
  module Private = struct
    include L.Private
    let config t = t.config
    let contents_t t = t.contents_t
    let node_t t = t.node_t
    let commit_t t = t.commit_t
    let tag_t t = t.tag_t
    let update_node t = t.update_node
    let read_node t = t.read_node
    let mem_node t = t.mem_node
  end
end
