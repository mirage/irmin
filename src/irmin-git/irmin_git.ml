(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

module Metadata = struct
  module X = struct
    type t = [`Normal | `Exec | `Link | `Everybody]
    let t = Irmin.Type.enum "metadata" [
        ("normal"   , `Normal);
        ("exec"     , `Exec);
        ("link"     , `Link);
        ("everybody", `Everybody);
      ]
  end
  include X
  let default = `Normal
  let merge = Irmin.Merge.default X.t
end

let src = Logs.Src.create "irmin.git" ~doc:"Irmin Git-format store"
module Log = (val Logs.src_log src : Logs.LOG)

module Conf = struct

  let root = Irmin.Private.Conf.root

  let reference =
    let parse str = Ok (Git.Reference.of_string str) in
    let print ppf name = Fmt.string ppf (Git.Reference.to_string name) in
    parse, print

  let head =
    Irmin.Private.Conf.key
      ~doc:"The main branch of the Git repository."
      "head" Irmin.Private.Conf.(some reference) None

  let bare =
    Irmin.Private.Conf.key
      ~doc:"Do not expand the filesystem on the disk."
      "bare" Irmin.Private.Conf.bool false

  let level =
    Irmin.Private.Conf.key
      ~doc:"The Zlib compression level."
      "level" Irmin.Private.Conf.(some int) None

  let buffers =
    Irmin.Private.Conf.key
      ~doc:"The number of 4K pre-allocated buffers."
      "buffers" Irmin.Private.Conf.(some int) None

  let dot_git =
    Irmin.Private.Conf.key
      ~doc:"The location of the .git directory. By default set to [$root/.git]."
      "dot-git" Irmin.Private.Conf.(some string) None

end

let config ?(config=Irmin.Private.Conf.empty) ?head ?bare ?level ?dot_git root =
  let module C = Irmin.Private.Conf in
  let config = C.add config Conf.root (Some root) in
  let config = match bare with
    | None   -> C.add config Conf.bare (C.default Conf.bare)
    | Some b -> C.add config Conf.bare b
  in
  let config = C.add config Conf.head head in
  let config = C.add config Conf.level level in
  let config = C.add config Conf.dot_git dot_git in
  config

module Make_private
    (G: Git.S)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
= struct

  module H = Irmin.Hash.Make(G.Hash)

  module type V = sig
    type t
    (* FIXME: use G.Value.kind *)
    val pp: t Fmt.t
    val type_eq: [`Commit | `Blob | `Tree | `Tag] -> bool
    val to_git: t -> G.Value.t
    val of_git: G.Value.t -> (t option, [`Msg of string]) result
  end

  module Content_addressable (V: V) = struct

    type 'a t = G.t
    type key = H.t
    type value = V.t

    let pp_key = Irmin.Type.pp H.t

    let mem t key =
      Log.debug (fun l -> l "mem %a" pp_key key);
      G.mem t key >>= function
      | false    -> Lwt.return false
      | true     ->
        G.read t key >>= function
        | Error `Not_found -> Lwt.return false
        | Error e          -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
        | Ok v             -> Lwt.return (V.type_eq (G.Value.kind v))

    let find t key =
      Log.debug (fun l -> l "find %a" pp_key key);
      G.read t key >>= function
      | Error `Not_found -> Lwt.return None
      | Error e          -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
      | Ok v             -> match V.of_git v with
        | Ok v           -> Lwt.return v
        | Error (`Msg e) -> Lwt.fail_with e

    let add t v =
      Log.debug (fun l -> l "add %a" V.pp v);
      G.write t (V.to_git v) >>= function
      | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
      | Ok k    -> Lwt.return (fst k)

  end

  let err fmt = Fmt.kstrf (fun e -> Error (`Msg e)) fmt

  module Raw = Git.Value.Raw(G.Hash)(G.Inflate)(G.Deflate)

  module GitContents = struct
    type t = C.t
    let pp = Irmin.Type.pp C.t
    let to_string = Irmin.Type.to_string C.t
    let type_eq = function `Blob -> true | _ -> false

    let of_string str = match Irmin.Type.of_string C.t str with
      | Ok x           -> Ok (Some x)
      | Error (`Msg e) -> err "Git.Contents: cannot parse %S: %s" str e

    let of_git = function
      | G.Value.Blob b -> of_string (G.Value.Blob.to_string b)
      | _              -> Ok None

    let to_git b = G.Value.blob (G.Value.Blob.of_string (to_string b))
  end
  module XContents = struct
    include Content_addressable (GitContents)
    module Val = struct
      include C

      let to_bin t =
        let blob = G.Value.Blob.of_string (Irmin.Type.encode_bin C.t t) in
        match Raw.to_raw (G.Value.blob blob) with
        | Error _ -> assert false
        | Ok s    -> s

      let encode_bin buf off (t:t) =
        Log.debug (fun l -> l "Content.encode_bin");
        let s = to_bin t in
        let len = String.length s in
        Bytes.blit_string s 0 buf off len;
        off + len

      let decode_bin buf off =
        Log.debug (fun l -> l "Content.decode_bin");
        let buf = Cstruct.of_string buf in
        let buf = Cstruct.shift buf off in
        let blob t =
          match Irmin.Type.decode_bin C.t (G.Value.Blob.to_string t) with
          | Ok t -> t
          | Error (`Msg e) -> Fmt.failwith "cannot read blob: %s" e
        in
        match Raw.of_raw_with_header buf with
        | Ok (G.Value.Blob t) -> off + Cstruct.len buf, blob t
        | Ok _    -> failwith "wrong object kind"
        | Error e -> Fmt.invalid_arg "error %a" Raw.DecoderRaw.pp_error e

      let size_of t = `Buffer (to_bin t)

    let t = Irmin.Type.like' ~bin:(encode_bin, decode_bin, size_of) t
  end
    module Key = H
  end
  module Contents = Irmin.Contents.Store(XContents)

  module XNode = struct
    module Key = H
    module Path = P

    module Val = struct
      module Metadata = Metadata

      type t = G.Value.Tree.t
      let pp = G.Value.Tree.pp
      type metadata = Metadata.t
      type hash = Key.t
      type step = Path.step
      type value = [`Node of hash | `Contents of hash * metadata ]
      let metadata_t = Metadata.t
      let hash_t = Key.t
      let step_t = Path.step_t

      let value_t =
        let open Irmin.Type in
        variant "Tree.value" (fun node contents -> function
            | `Node n     -> node n
            | `Contents c -> contents c)
        |~ case1 "node"     hash_t     (fun n -> `Node n)
        |~ case1 "contents" (pair hash_t metadata_t) (fun c -> `Contents c)
        |> sealv

      let of_step = Irmin.Type.to_string P.step_t

      let to_step str = match Irmin.Type.of_string P.step_t str with
        | Ok x           -> x
        | Error (`Msg e) -> failwith e

      let list t =
        List.fold_left (fun acc { G.Value.Tree.perm; name; node } ->
            let name = to_step name in
            match perm with
            | `Dir             -> (name, `Node node) :: acc
            | `Commit          -> acc (* FIXME *)
            | #Metadata.t as p -> (name, `Contents (node, p)) :: acc
          ) [] (G.Value.Tree.to_list t)
        |> List.rev

      let find t s =
        let s = of_step s in
        let rec aux = function
          | [] -> None
          | x::xs when x.G.Value.Tree.name <> s -> aux xs
          | { G.Value.Tree.perm; node; _ } :: _ ->
            match perm with
            | `Dir             -> Some (`Node node)
            | `Commit          -> None (* FIXME *)
            | #Metadata.t as p -> Some (`Contents (node, p))
        in
        aux (G.Value.Tree.to_list t)

      let remove t step = G.Value.Tree.remove ~name:(of_step step) t
      let is_empty = G.Value.Tree.is_empty

      let update t name value =
        let entry = match value with
          | `Node node -> G.Value.Tree.entry (of_step name) `Dir node
          | `Contents (node, perm) ->
            G.Value.Tree.entry (of_step name) (perm:>G.Value.Tree.perm) node
        in
        G.Value.Tree.add t entry

      let empty = G.Value.Tree.of_list []

      let to_git perm (name, node) = G.Value.Tree.entry (of_step name) perm node

      let v alist =
        let alist = List.rev_map (fun (l, x) ->
            let v k = l, k in
            match x with
            | `Node n             -> to_git `Dir (v n)
            | `Contents (c, perm) -> to_git (perm :> G.Value.Tree.perm) (v c)
          ) alist
        in
        G.Value.Tree.of_list alist

      let alist t =
        let mk_n k = `Node k in
        let mk_c k metadata = `Contents (k, metadata) in
        List.fold_left (fun acc -> function
            | { G.Value.Tree.perm = `Dir; name; node } ->
              (to_step name, mk_n node) :: acc
            | { G.Value.Tree.perm = `Commit; name; _ } ->
              (* Irmin does not support Git submodules; do not follow them,
                 just consider *)
              Log.warn (fun l -> l "skipping Git submodule: %s" name);
              acc
            | { G.Value.Tree.perm = #Metadata.t as perm; name; node; _ } ->
              (to_step name, mk_c node perm) :: acc
          ) [] (G.Value.Tree.to_list t)

       module N = Irmin.Private.Node.Make(H)(P)(Metadata)
       let to_n t = N.v (alist t)
       let of_n n = v (N.list n)

       let to_bin t =
         match Raw.to_raw (G.Value.tree t) with
         | Error _ -> assert false
         | Ok s    -> s

       let encode_bin buf off (t:t) =
         Log.debug (fun l -> l "Tree.encode_bin");
         let s = to_bin t in
         let len = String.length s in
         Bytes.blit_string s 0 buf off len;
           off + len

       let decode_bin buf off =
         Log.debug (fun l -> l "Tree.decode_bin");
         let buf = Cstruct.of_string buf in
         let buf = Cstruct.shift buf off in
         match Raw.of_raw_with_header buf with
         | Ok (G.Value.Tree t) -> off + Cstruct.len buf, t
         | Ok _    -> failwith "wrong object kind"
         | Error e -> Fmt.invalid_arg "error %a" Raw.DecoderRaw.pp_error e

       let size_of t = `Buffer (to_bin t)

       let t =
         Irmin.Type.like ~bin:(encode_bin, decode_bin, size_of) N.t of_n to_n
    end

    include Content_addressable (struct
        type t = Val.t
        let pp = Val.pp
        let type_eq = function `Tree -> true | _ -> false
        let to_git t = G.Value.tree t
        let of_git = function G.Value.Tree t -> Ok (Some t) | _ -> Ok None
      end)
  end
  module Node = Irmin.Private.Node.Store(Contents)(P)(Metadata)(XNode)

  module XCommit = struct
    module Val = struct
      type t = G.Value.Commit.t
      let pp = G.Value.Commit.pp

      type hash = H.t

      let hash_t = H.t

      let info_of_git author message =
        let id = author.Git.User.name in
        let date, _ = author.Git.User.date in (* FIXME: tz offset is ignored *)
        Irmin.Info.v ~date ~author:id message

      let name_email name =
        let name = String.trim name in
        try
          let i = String.rindex name ' ' in
          let email = String.sub name (i+1) (String.length name - i - 1) in
          if String.length email > 0
             && email.[0] = '<'
             && email.[String.length email - 1] = '>'
          then
            let email = String.sub email 1 (String.length email - 2) in
            let name = String.trim (String.sub name 0 i) in
            name, email
          else
            name, "irmin@openmirage.org"
        with Not_found ->
          name, "irmin@openmirage.org"

      let of_git g =
        let node = G.Value.Commit.tree g in
        let parents = G.Value.Commit.parents g in
        let author = G.Value.Commit.author g in
        let message = G.Value.Commit.message g in
        let message =
          if String.length message > 0 && String.get message 0 = '\n' then
            String.sub message 1 (String.length message - 1)
          else
            message
        in
        let info = info_of_git author message in
        (info, node, parents)

      let to_git info node parents =
        let tree = node in
        let parents = List.fast_sort G.Hash.compare parents in
        let author =
          let date = Irmin.Info.date info in
          let name, email = name_email (Irmin.Info.author info) in
          Git.User.({ name; email;
                      date  = date, None;
                    }) in
        let message = Irmin.Info.message info in
        let message = if message = "" then "" else  "\n" ^ message in
        G.Value.Commit.make (* FIXME: should be v *)
          ~tree ~parents ~author ~committer:author message

      let v ~info ~node ~parents = to_git info node parents
      let xnode g = G.Value.Commit.tree g
      let node t = xnode t
      let parents g = G.Value.Commit.parents g
      let info g =
        let author = G.Value.Commit.author g in
        let message = G.Value.Commit.message g in
        info_of_git author message

      module C = Irmin.Private.Commit.Make(H)

      let of_c c = to_git (C.info c) (C.node c) (C.parents c)

      let to_c t =
        let info, node, parents = of_git t in
        C.v ~info ~node ~parents

      let to_bin t =
        match Raw.to_raw (G.Value.commit t) with
        | Error _ -> assert false
        | Ok s    -> s

      let encode_bin buf off (t:t) =
        Log.debug (fun l -> l "Commit.encode_bin");
        let s = to_bin t in
        let len = String.length s in
        Bytes.blit_string s 0 buf off len;
        off + len

      let decode_bin buf off =
        Log.debug (fun l -> l "Commit.decode_bin");
        let buf = Cstruct.of_string buf in
        let buf = Cstruct.shift buf off in
        match Raw.of_raw_with_header buf with
        | Ok (G.Value.Commit t) -> off + Cstruct.len buf, t
        | Ok _    -> failwith "wrong object kind"
        | Error e -> Fmt.invalid_arg "error %a" Raw.DecoderRaw.pp_error e

      let size_of t = `Buffer (to_bin t)

      let t =
        Irmin.Type.like ~bin:(encode_bin, decode_bin, size_of) C.t of_c to_c
    end

    module Key = H

    include Content_addressable (struct
        type t = Val.t
        let pp = Val.pp
        let type_eq = function `Commit -> true | _ -> false
        let of_git = function G.Value.Commit c -> Ok (Some c) | _ -> Ok None
        let to_git c = G.Value.commit c
      end)

  end
  module Commit = Irmin.Private.Commit.Store(Node)(XCommit)

end

module type BRANCH = sig
  include Irmin.Branch.S
  val pp_ref: t Fmt.t
  val of_ref: string -> (t, [`Msg of string]) result
end

module Branch (B: Irmin.Branch.S): BRANCH with type t = B.t = struct
  open Astring
  include B
  let pp = Irmin.Type.pp B.t
  let pp_ref ppf b = Fmt.pf ppf "refs/heads/%a" pp b

  let of_ref str = match String.cuts ~sep:"/" str with
    | "refs" :: "heads" :: b -> Irmin.Type.of_string B.t (String.concat ~sep:"/" b)
    | _ -> Error (`Msg (Fmt.strf "%s is not a valid branch" str))
end


module Irmin_branch_store
    (G: Git.S)
    (B: BRANCH)
= struct

  module Key = B
  module Val = Irmin.Hash.Make(G.Hash)

  module W = Irmin.Private.Watch.Make(Key)(Val)

  type t = {
    bare: bool;
    dot_git: Fpath.t;
    git_head: G.Reference.head_contents;
    t: G.t;
    w: W.t;
    m: Lwt_mutex.t;
  }

  module E = Ephemeron.K1.Make (struct
    type t = Fpath.t
    let equal = fun x y -> compare x y = 0
    let hash = Hashtbl.hash
  end)

  let watches = E.create 10

  type key = Key.t
  type value = Val.t
  type watch = W.watch * (unit -> unit Lwt.t)

  let branch_of_git r =
    let str = String.trim @@ G.Reference.to_string r in
    match B.of_ref str with
    | Ok r           -> Some r
    | Error (`Msg _) -> None

  let git_of_branch r = G.Reference.of_string (Fmt.to_to_string B.pp_ref r)

  let pp_key = Irmin.Type.pp Key.t

  let mem { t; _ } r =
    Log.debug (fun l -> l "mem %a" pp_key r);
    G.Ref.mem t (git_of_branch r)

  let find { t; _ } r =
    Log.debug (fun l -> l "find %a" pp_key r);
    G.Ref.resolve t (git_of_branch r) >>= function
    | Error `Not_found -> Lwt.return None
    | Error e          -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
    | Ok  k            -> Lwt.return (Some k)

  let listen_dir t =
    let (/) = Filename.concat in
    if G.has_global_watches then
      let dir = Fpath.(to_string @@ t.dot_git / "refs") in
      let key file = match B.of_ref ("refs" / file) with
        | Ok x           -> Some x
        | Error (`Msg e) ->
          Log.err (fun l -> l "listen: file %s: %s" file e);
          None
      in
      W.listen_dir t.w dir ~key ~value:(find t)
    else
      Lwt.return (fun () -> Lwt.return_unit)

  let watch_key t key ?init f =
    Log.debug (fun l -> l "watch_key %a" pp_key key);
    listen_dir t >>= fun stop ->
    W.watch_key t.w key ?init f >|= fun w ->
    (w, stop)

  let watch t ?init f =
    Log.debug (fun l -> l "watch");
    listen_dir t >>= fun stop ->
    W.watch t.w ?init f >|= fun w ->
    (w, stop)

  let unwatch t (w, stop) =
    stop () >>= fun () ->
    W.unwatch t.w w

  let v ?lock ~head ~bare t =
    let m = match lock with None -> Lwt_mutex.create () | Some l -> l in
    let dot_git = G.dotgit t in
    let write_head head =
      let head = G.Reference.Ref head in
      ((if G.has_global_checkout
        then Lwt_mutex.with_lock m (fun () -> G.Ref.write t G.Reference.head head)
        else Lwt.return (Ok ()))
       >|= function
       | Error e -> Log.err (fun l -> l "Cannot create HEAD: %a" G.pp_error e)
       | Ok  ()  -> ()) >|= fun () ->
      head
    in
    begin match head with
      | Some h -> write_head h
      | None   ->
        G.Ref.read t G.Reference.head >>= function
        | Error `Not_found -> write_head (git_of_branch B.master)
        | Error e          -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
        | Ok r             -> Lwt.return r
    end >|= fun git_head ->
    let w =
      try E.find watches (G.dotgit t)
      with Not_found ->
        let w = W.v () in
        E.add watches (G.dotgit t) w;
        w
    in
    { git_head; bare; t; w; dot_git; m }

  let list { t; _ } =
    Log.debug (fun l -> l "list");
    G.Ref.list t >|= fun refs ->
    List.fold_left (fun acc (r, _) ->
        match branch_of_git r with
        | None   -> acc
        | Some r -> r :: acc
      ) [] refs

  let write_index t gr gk =
    Log.debug (fun l -> l "write_index");
    if G.has_global_checkout then
      Log.debug (fun f -> f "write_index");
    let git_head = G.Reference.Ref gr in
    Log.debug (fun f -> f "write_index/if bare=%b head=%a" t.bare G.Reference.pp gr);
    if not t.bare && git_head = t.git_head then (
      Log.debug (fun f -> f "write cache (%a)" G.Reference.pp gr);
      (* FIXME G.write_index t.t gk *)
      let _ = gk in Lwt.return_unit
    ) else
      Lwt.return_unit

  let pp_branch = Irmin.Type.pp B.t

  let set t r k =
    Log.debug (fun f -> f "set %a" pp_branch r);
    let gr = git_of_branch r in
    Lwt_mutex.with_lock t.m (fun () -> G.Ref.write t.t gr (G.Reference.Hash k))
    >>= function
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
    | Ok ()    ->
      W.notify t.w r (Some k) >>= fun () ->
      write_index t gr k

  let remove t r =
    Log.debug (fun f -> f "remove %a" pp_branch r);
    Lwt_mutex.with_lock t.m (fun () -> G.Ref.remove t.t (git_of_branch r))
    >>= function
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
    | Ok ()   -> W.notify t.w r None

  let eq_head_contents_opt x y = match x, y with
    | None  , None   -> true
    | Some x, Some y -> G.Reference.equal_head_contents x y
    | _ -> false

  let test_and_set t r ~test ~set =
    Log.debug (fun f -> f "test_and_set %a" pp_branch r);
    let gr = git_of_branch r in
    let c = function
      | None   -> None
      | Some h -> Some (G.Reference.Hash h)
    in
    let ok = function
      | Ok ()   -> Lwt.return true
      | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
    in
    Lwt_mutex.with_lock t.m (fun () ->
        (G.Ref.read t.t gr >>= function
          | Error `Not_found -> Lwt.return None
          | Ok x             -> Lwt.return (Some x)
          | Error e          -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
        ) >>= fun x ->
        (if not (eq_head_contents_opt x (c test)) then Lwt.return false
         else match c set with
           | None   -> G.Ref.remove t.t gr  >>= ok
           | Some h -> G.Ref.write t.t gr h >>= ok
        ) >>= fun b ->
        (if b then W.notify t.w r set else Lwt.return_unit) >>= fun () ->
        begin
          (* We do not protect [write_index] because it can take a log
             time and we don't want to hold the lock for too long. Would
             be safer to grab a lock, although the expanded filesystem
             is not critical for Irmin consistency (it's only a
             convenience for the user). *)
          if b then match set with
            | None   -> Lwt.return_unit
            | Some v -> write_index t gr v
          else
            Lwt.return_unit
        end >|= fun () ->
        b
      )

end

module Irmin_sync_store
    (G: Git.S)
    (S: Git.Sync.S with module Store := G)
    (B: Irmin.Branch.S) =
struct

  module H = Irmin.Hash.Make(G.Hash)

  type t = G.t
  type commit = H.t
  type branch = B.t

  type endpoint = S.Endpoint.t

  let git_of_branch_str str = G.Reference.of_string ("refs/heads/" ^ str)
  let git_of_branch r = git_of_branch_str (Irmin.Type.to_string B.t r)

  let o_head_of_git = function
    | None   -> Error `No_head
    | Some k -> Ok (Some k)

  let fetch t ?depth e br =
    let uri = S.Endpoint.uri e in
    Log.debug (fun f -> f "fetch %a" Uri.pp_hum uri);
    let _deepen = depth in (* FIXME: need to be exposed in the Git API *)
    let reference = git_of_branch br in
    let result refs =
      (* FIXME: need pp_result *)
      Log.debug (fun f -> f "fetch result: XXX" (* Git.Sync.Result.pp_fetch r *));
      let key =
        try Some (G.Reference.Map.find reference refs)
        with Not_found -> None
      in
      o_head_of_git key
    in
    let references =
      (* remote *)
      reference,
      (* local *)
      [ G.Reference.of_string
          ("refs/remotes/origin/" ^ (Irmin.Type.to_string B.t br));
        reference ]
    in
    S.fetch_one t e ~reference:references >|= function
    | Error e -> Fmt.kstrf (fun e -> Error (`Msg e)) "%a" S.pp_error e
    | Ok (`Sync refs) -> result refs
    | Ok `AlreadySync -> Ok None

  let push t ?depth:_ e br =
    let uri = S.Endpoint.uri e in
    Log.debug (fun f -> f "push %a" Uri.pp_hum uri);
    let reference = git_of_branch br in
    let result refs =
      (* FIXME: needs pp_push *)
      Log.debug (fun f -> f "push result: XX" (*Git.Sync.Result.pp_push r*));
      let errors = ref [] in
      List.iter (function
          | Ok _         -> ()
          | Error (r, e) ->
            errors := Fmt.strf "%a: %s" G.Reference.pp r e :: !errors
        ) refs;
      if !errors = [] then Ok ()
      else Fmt.kstrf (fun e -> Error (`Msg e)) "%a"
          Fmt.(list ~sep:(unit "@.") string) !errors
    in
    let references = G.Reference.Map.singleton
        (* local *)  reference
        (* remote *) [reference ]
    in
    S.update_and_create t ~references e >|= function
    | Error e -> Fmt.kstrf (fun e -> Error (`Msg e)) "%a" S.pp_error e
    | Ok r    -> result r

end

type reference = [
  | `Branch of string
  | `Remote of string
  | `Tag of string
  | `Other of string
]

module Reference: BRANCH with type t = reference = struct

  open Astring

  type t = [
    | `Branch of string
    | `Remote of string
    | `Tag of string
    | `Other of string
  ]

  let pp_ref ppf = function
    | `Branch b -> Fmt.pf ppf "refs/heads/%s" b
    | `Remote r -> Fmt.pf ppf "refs/remotes/%s" r
    | `Tag t    -> Fmt.pf ppf "refs/tags/%s" t
    | `Other o  -> Fmt.pf ppf "refs/%s" o

  let path l = String.concat ~sep:"/" l

  let of_ref str = match String.cuts ~sep:"/" str with
    | "refs" :: "heads"   :: b -> Ok (`Branch (path b))
    | "refs" :: "remotes" :: r -> Ok (`Remote (path r))
    | "refs" :: "tags"    :: t -> Ok (`Tag (path t))
    | "refs" :: o              -> Ok (`Other (path o))
    | _ -> Error (`Msg (Fmt.strf "%s is not a valid reference" str))

  let t =
    let open Irmin.Type in
    variant "reference" (fun branch remote tag other -> function
        | `Branch x -> branch x
        | `Remote x -> remote x
        | `Tag x    -> tag x
        | `Other x  -> other x)
    |~ case1 "branch" string (fun t -> `Branch t)
    |~ case1 "remote" string (fun t -> `Remote t)
    |~ case1 "tag"    string (fun t -> `Tag t)
    |~ case1 "other"  string (fun t -> `Other t)
    |> sealv

  let t = Irmin.Type.like' t ~cli:(pp_ref, of_ref)

  let master = `Branch Irmin.Branch.String.master

  let is_valid = function
    | `Branch s | `Tag s | `Remote s | `Other s ->
      Irmin.Branch.String.is_valid s

end

module type S = sig
  module Git: Git.S
  include Irmin.S with type metadata = Metadata.t
                   and type hash = Git.Hash.t
  val git_commit: Repo.t -> commit -> Git.Value.Commit.t option Lwt.t
  val git_of_repo: Repo.t -> Git.t
  val repo_of_git: ?head:Git.Reference.t -> ?bare:bool -> ?lock:Lwt_mutex.t ->
    Git.t -> Repo.t Lwt.t
end

module type G = sig
  include Git.S
  val v:
    ?dotgit:Fpath.t ->
    ?compression:int ->
    ?buffers:buffer Lwt_pool.t ->
    Fpath.t -> (t, error) result Lwt.t
end

module Make_ext
    (G: G)
    (S: Git.Sync.S with module Store := G)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: BRANCH)
= struct

  module R = Irmin_branch_store(G)(B)

  type r = {
    config: Irmin.config;
    g: G.t;
    b: R.t;
  }

  module P = struct
    module Hash = Irmin.Hash.Make(G.Hash)
    module XSync = struct
      include Irmin_sync_store(G)(S)(R.Key)
      let v repo = Lwt.return repo.g
    end
    include Make_private(G)(C)(P)
    module Branch = R
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = XSync
    module Repo = struct
      type t = r
      let branch_t t = t.b
      let contents_t t = t.g
      let node_t t = contents_t t, t.g
      let commit_t t = node_t t, t.g

      let batch t f = f (contents_t t) (node_t t) (commit_t t)

      type config = {
        root   : string;
        dot_git: string option;
        level  : int option;
        buffers: int option;
        head   : G.Reference.t option;
        bare   : bool;
      }

      let config c =
        let root = match Irmin.Private.Conf.get c Conf.root with
          | None   -> Sys.getcwd ()
          | Some d -> d
        in
        let dot_git = Irmin.Private.Conf.get c Conf.dot_git in
        let level = Irmin.Private.Conf.get c Conf.level in
        let head = Irmin.Private.Conf.get c Conf.head in
        let bare = Irmin.Private.Conf.get c Conf.bare in
        let buffers = Irmin.Private.Conf.get c Conf.buffers in
        { root; dot_git; level; head; buffers; bare }

      let fopt f = function None -> None | Some x -> Some (f x)

      let v conf =
        let { root; dot_git; level; head; bare; buffers } = config conf in
        let dotgit = fopt Fpath.v dot_git in
        let root = Fpath.v root in
        let buffers =
          fopt (fun n -> Lwt_pool.create n (fun () ->
              Lwt.return (G.buffer ()))
            ) buffers
        in
        G.v ?dotgit ?compression:level ?buffers root >>= function
        | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
        | Ok g    ->
          R.v ~head ~bare g >|= fun b ->
          { g; b; config = conf }

    end
  end

  include Irmin.Of_private(P)

  module Git = G

  let git_commit (repo:Repo.t) (h:commit): Git.Value.Commit.t option Lwt.t =
    let h = Commit.hash h in
    Git.read repo.g h >|= function
    | Ok Git.Value.Commit c -> Some c
    | _ -> None

  let git_of_repo r = r.g

  let repo_of_git ?head ?(bare=true) ?lock g =
    R.v ?lock ~head ~bare g >|= fun b ->
    { config = Irmin.Private.Conf.empty; g; b }

end

module Mem = struct

  include Git.Mem.Store

  module E = Ephemeron.K1.Make (struct
    type t = Fpath.t  * Fpath.t option * int option * buffer Lwt_pool.t option
    let equal = fun x y -> compare x y = 0
    let hash = Hashtbl.hash
  end)

  let confs = E.create 10

  let find_conf c = match E.find confs c with
    | exception Not_found -> None
    | x -> Some x

  let add_conf c t = E.replace confs c t; t

  let v' ?dotgit ?compression ?buffers root =
    let buffer = match buffers with
      | None   -> None
      | Some p -> Some (Lwt_pool.use p)
    in
    v ?dotgit ?compression ?buffer root

  let v ?dotgit ?compression ?buffers root =
    let conf = (root, dotgit, compression, buffers) in
    match find_conf conf with
    | Some x -> Lwt.return x
    | None   ->
      v' ?dotgit ?compression ?buffers root >|= add_conf conf

end

module Make
    (G  : G)
    (S  : Git.Sync.S with module Store := G)
    (C  : Irmin.Contents.S)
    (P  : Irmin.Path.S)
    (B  : Irmin.Branch.S)
  =
  Make_ext (G)(S)(C)(P)(Branch(B))

module No_sync (G: Git.S) = struct
  (* XXX(samoht): so much boilerplate... *)
  module Store = G
  module Endpoint = struct
    type t = unit
    let uri _ = assert false
  end
  type error = unit
  let pp_error _ _ = assert false

  type command =
    [ `Create of Store.Hash.t * Store.Reference.t
    | `Delete of Store.Hash.t * Store.Reference.t
    | `Update of Store.Hash.t * Store.Hash.t * Store.Reference.t ]

  let pp_command _ _ = assert false
  let push _ = assert false
  let ls _ = assert false
  let fetch _ = assert false
  let fetch_one _ = assert false
  let fetch_some _ = assert false
  let fetch_all _ = assert false
  let clone _ = assert false
  let update_and_create _ = assert false
end

module Content_addressable (G: Git.S) (V: Irmin.Type.S) = struct
  module G = struct
    include G
    let v ?dotgit:_ ?compression:_ ?buffers:_ _root =
      assert false
  end
  module V = struct
    include V
    let merge = Irmin.Merge.default Irmin.Type.(option V.t)
  end
  module M = Make_ext (G)(No_sync(G))(V)(Irmin.Path.String_list)(Reference)
  module X = M.Private.Contents
  let state t =
    M.repo_of_git t >|= fun r ->
    M.Private.Repo.contents_t r
  type 'a t = G.t
  type key = X.key
  type value = X.value
  let with_state f t x = state t >>= fun t -> f t x
  let add = with_state X.add
  let find = with_state X.find
  let mem = with_state X.mem
end

module Atomic_write (G: Git.S) (K: Irmin.Branch.S) =
struct
  module K = struct
    include K
    let master = match Irmin.Type.of_string K.t "master" with
      | Ok x           -> x
      | Error (`Msg e) -> failwith e
  end
  module B = Branch(K)
  include Irmin_branch_store (G)(B)
end

module KV
    (G: G)
    (S: Git.Sync.S with module Store := G)
    (C: Irmin.Contents.S)
  = Make (G)(S)(C)(Irmin.Path.String_list)(Irmin.Branch.String)

module Ref
    (G: G)
    (S: Git.Sync.S with module Store := G)
    (C: Irmin.Contents.S)
  = Make_ext (G)(S)(C)(Irmin.Path.String_list)(Reference)

module type S_MAKER = functor
    (G: G)
    (S: Git.Sync.S with module Store := G)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S) ->
    S with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and module Git = G
       and type Private.Sync.endpoint = S.Endpoint.t

module type KV_MAKER = functor
  (G: G)
  (S: Git.Sync.S with module Store := G)
  (C: Irmin.Contents.S) ->
    S with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = string
       and module Git = G
       and type Private.Sync.endpoint = S.Endpoint.t

module type REF_MAKER = functor
  (G: G)
  (S: Git.Sync.S with module Store := G)
  (C: Irmin.Contents.S) ->
    S with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = reference
       and module Git = G
       and type Private.Sync.endpoint = S.Endpoint.t

include Conf

module Generic
    (CA: Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW: Irmin.ATOMIC_WRITE_STORE_MAKER)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
= struct

  (* We use a dummy store to get the serialisation functions. This is
     probably not necessary and we could use Git.Value.Raw instead. *)
  module G = Mem
  module S = Make(G)(No_sync(G))(C)(P)(B)

  include
    Irmin.Make_ext(CA)(AW)
      (S.Private.Node.Metadata)
      (S.Private.Contents.Val)
      (S.Private.Node.Path)
      (S.Branch)
      (S.Private.Contents.Key)
      (S.Private.Node.Val)
      (S.Private.Commit.Val)
end

module Generic_KV
    (CA: Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW: Irmin.ATOMIC_WRITE_STORE_MAKER)
    (C : Irmin.Contents.S)
  = Generic (CA)(AW)(C)(Irmin.Path.String_list)(Irmin.Branch.String)
