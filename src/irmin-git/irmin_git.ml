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

open! Import

module Metadata = struct
  module X = struct
    type t = [ `Normal | `Exec | `Link | `Everybody ]

    let t =
      Irmin.Type.enum "metadata"
        [
          ("normal", `Normal);
          ("exec", `Exec);
          ("link", `Link);
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
    let parse str = Git.Reference.of_string str in
    let print ppf name = Fmt.string ppf (Git.Reference.to_string name) in
    (parse, print)

  let head =
    Irmin.Private.Conf.key ~doc:"The main branch of the Git repository." "head"
      Irmin.Private.Conf.(some reference)
      None

  let bare =
    Irmin.Private.Conf.key ~doc:"Do not expand the filesystem on the disk."
      "bare" Irmin.Private.Conf.bool false

  let level =
    Irmin.Private.Conf.key ~doc:"The Zlib compression level." "level"
      Irmin.Private.Conf.(some int)
      None

  let buffers =
    Irmin.Private.Conf.key ~doc:"The number of 4K pre-allocated buffers."
      "buffers"
      Irmin.Private.Conf.(some int)
      None

  let dot_git =
    Irmin.Private.Conf.key
      ~doc:"The location of the .git directory. By default set to [$root/.git]."
      "dot-git"
      Irmin.Private.Conf.(some string)
      None
end

let config ?(config = Irmin.Private.Conf.empty) ?head ?bare ?level ?dot_git root
    =
  let module C = Irmin.Private.Conf in
  let config = C.add config Conf.root (Some root) in
  let config =
    match bare with
    | None -> C.add config Conf.bare (C.default Conf.bare)
    | Some b -> C.add config Conf.bare b
  in
  let config = C.add config Conf.head head in
  let config = C.add config Conf.level level in
  let config = C.add config Conf.dot_git dot_git in
  config

(** NOTE(craigfe): As of Git 2.1.3, attempting to [reset] repositories
    concurrently can fail due to file-system race conditions. The next version
    should fix this issue, so this global lock is a quick workaround. *)
let reset_lock = Lwt_mutex.create ()

module Make_private (G : Git.S) (C : Irmin.Contents.S) (P : Irmin.Path.S) =
struct
  module H = Irmin.Hash.Make (G.Hash)

  let handle_git_err = function
    | Ok x -> Lwt.return x
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e

  module type V = sig
    type t

    val type_eq : [ `Commit | `Blob | `Tree | `Tag ] -> bool
    val to_git : t -> G.Value.t
    val of_git : G.Value.t -> t option
  end

  module Content_addressable (V : V) = Closeable.Content_addressable (struct
    type 'a t = G.t
    type key = H.t
    type value = V.t

    let pp_key = Irmin.Type.pp H.t

    let mem t key =
      Log.debug (fun l -> l "mem %a" pp_key key);
      G.mem t key >>= function
      | false -> Lwt.return_false
      | true -> (
          G.read t key >>= function
          | Error (`Reference_not_found _ | `Not_found _) -> Lwt.return_false
          | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
          | Ok v -> Lwt.return (V.type_eq (G.Value.kind v)))

    let find t key =
      Log.debug (fun l -> l "find %a" pp_key key);
      G.read t key >>= function
      | Error (`Reference_not_found _ | `Not_found _) -> Lwt.return_none
      | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
      | Ok v -> Lwt.return (V.of_git v)

    let add t v =
      let v = V.to_git v in
      let* k, _ = G.write t v >>= handle_git_err in
      Log.debug (fun l -> l "add %a" pp_key k);
      Lwt.return k

    let equal_hash = Irmin.Type.(unstage (equal H.t))

    let unsafe_add t k v =
      let+ k' = add t v in
      if equal_hash k k' then ()
      else
        Fmt.failwith
          "[Git.unsafe_append] %a is not a valid key. Expecting %a instead.\n"
          pp_key k pp_key k'

    let clear t =
      Log.debug (fun l -> l "clear");
      Lwt_mutex.with_lock reset_lock (fun () -> G.reset t) >>= handle_git_err
  end)

  module Raw = Git.Value.Make (G.Hash)

  module XContents = struct
    module GitContents = struct
      type t = C.t

      let type_eq = function `Blob -> true | _ -> false

      let of_git = function
        | Git.Value.Blob b -> (
            let str = G.Value.Blob.to_string b in
            match Irmin.Type.of_string C.t str with
            | Ok x -> Some x
            | Error (`Msg e) -> Fmt.invalid_arg "error %s" e)
        | _ -> None

      let to_git b =
        let str = Irmin.Type.to_string C.t b in
        G.Value.blob (G.Value.Blob.of_string str)
    end

    include Content_addressable (GitContents)

    module Val = struct
      include C

      let to_bin t = Raw.to_raw (GitContents.to_git t)
      let encode_bin = Irmin.Type.stage (fun (t : t) k -> k (to_bin t))

      let decode_bin =
        Irmin.Type.stage @@ fun buf off ->
        Log.debug (fun l -> l "Content.decode_bin");
        match Raw.of_raw_with_header ~off buf with
        | Ok g -> (
            match GitContents.of_git g with
            | Some g -> (String.length buf, g)
            | None -> failwith "wrong object kind")
        | Error (`Msg _) -> failwith "wrong object"

      let size_of = Irmin.Type.Size.custom_dynamic ()
      let t = Irmin.Type.like ~bin:(encode_bin, decode_bin, size_of) t
    end

    module Key = H
  end

  module Contents = Irmin.Contents.Store (XContents)

  module XNode = struct
    module Key = H
    module Path = P

    module Val = struct
      module Metadata = Metadata

      type t = G.Value.Tree.t
      type metadata = Metadata.t [@@deriving irmin]
      type hash = Key.t [@@deriving irmin]
      type step = Path.step [@@deriving irmin]

      type value = [ `Node of hash | `Contents of hash * metadata ]
      [@@deriving irmin]

      let default = Metadata.default
      let of_step = Irmin.Type.to_string P.step_t

      let to_step str =
        match Irmin.Type.of_string P.step_t str with
        | Ok x -> x
        | Error (`Msg e) -> failwith e

      exception Exit of (step * value) list

      let list ?(offset = 0) ?length t =
        let t = G.Value.Tree.to_list t in
        let length = match length with None -> List.length t | Some n -> n in
        try
          List.fold_left
            (fun (i, acc) { Git.Tree.perm; name; node } ->
              if i < offset then (i + 1, acc)
              else if i >= offset + length then raise (Exit acc)
              else
                let name = to_step name in
                match perm with
                | `Dir -> (i + 1, (name, `Node node) :: acc)
                | `Commit -> (i + 1, acc) (* FIXME *)
                | #Metadata.t as p -> (i + 1, (name, `Contents (node, p)) :: acc))
            (0, []) t
          |> fun (_, acc) -> List.rev acc
        with Exit acc -> List.rev acc

      let find t s =
        let s = of_step s in
        let rec aux = function
          | [] -> None
          | x :: xs when x.Git.Tree.name <> s -> aux xs
          | { Git.Tree.perm; node; _ } :: _ -> (
              match perm with
              | `Dir -> Some (`Node node)
              | `Commit -> None (* FIXME *)
              | #Metadata.t as p -> Some (`Contents (node, p)))
        in
        aux (Git.Tree.to_list t)

      let remove t step = G.Value.Tree.remove ~name:(of_step step) t
      let is_empty = G.Value.Tree.is_empty
      let length t = G.Value.Tree.length t |> Int64.to_int

      let add t name value =
        let name = of_step name in
        let entry =
          match value with
          | `Node node -> Git.Tree.entry ~name `Dir node
          | `Contents (node, perm) ->
              Git.Tree.entry ~name (perm :> Git.Tree.perm) node
        in
        (* FIXME(samoht): issue in G.Value.Tree.add *)
        let entries = G.Value.Tree.to_list t in
        match List.find (fun e -> e.Git.Tree.name = name) entries with
        | exception Not_found -> Git.Tree.of_list (entry :: entries)
        | e ->
            let equal x y =
              x.Git.Tree.perm = y.Git.Tree.perm
              && x.name = y.name
              && G.Hash.equal x.node y.node
            in
            if equal e entry then t
            else
              let entries =
                List.filter (fun e -> e.Git.Tree.name <> name) entries
              in
              Git.Tree.of_list (entry :: entries)

      let empty = Git.Tree.of_list []

      let to_git perm (name, node) =
        G.Value.Tree.entry ~name:(of_step name) perm node

      let v alist =
        let alist =
          List.rev_map
            (fun (l, x) ->
              let v k = (l, k) in
              match x with
              | `Node n -> to_git `Dir (v n)
              | `Contents (c, perm) -> to_git (perm :> Git.Tree.perm) (v c))
            alist
        in
        (* Tree.of_list will sort the list in the right order *)
        G.Value.Tree.of_list alist

      let alist t =
        let mk_n k = `Node k in
        let mk_c k metadata = `Contents (k, metadata) in
        List.fold_left
          (fun acc -> function
            | { Git.Tree.perm = `Dir; name; node } ->
                (to_step name, mk_n node) :: acc
            | { Git.Tree.perm = `Commit; name; _ } ->
                (* Irmin does not support Git submodules; do not follow them,
                   just consider *)
                Log.warn (fun l -> l "skipping Git submodule: %s" name);
                acc
            | { Git.Tree.perm = #Metadata.t as perm; name; node; _ } ->
                (to_step name, mk_c node perm) :: acc)
          [] (G.Value.Tree.to_list t)
        |> List.rev

      module N = Irmin.Private.Node.Make (H) (P) (Metadata)

      let to_n t = N.v (alist t)
      let of_n n = v (N.list n)
      let to_bin t = Raw.to_raw (G.Value.tree t)

      let encode_bin =
        Irmin.Type.stage @@ fun (t : t) k ->
        Log.debug (fun l -> l "Tree.encode_bin");
        k (to_bin t)

      let decode_bin =
        Irmin.Type.stage @@ fun buf off ->
        Log.debug (fun l -> l "Tree.decode_bin");
        match Raw.of_raw_with_header buf ~off with
        | Ok (Git.Value.Tree t) -> (String.length buf, t)
        | Ok _ -> failwith "wrong object kind"
        | Error _ -> failwith "wrong object"

      let size_of = Irmin.Type.Size.custom_dynamic ()

      let t =
        Irmin.Type.map ~bin:(encode_bin, decode_bin, size_of) N.t of_n to_n
    end

    include Content_addressable (struct
      type t = Val.t

      let type_eq = function `Tree -> true | _ -> false
      let to_git t = G.Value.tree t
      let of_git = function Git.Value.Tree t -> Some t | _ -> None
    end)
  end

  module Node = Irmin.Private.Node.Store (Contents) (P) (Metadata) (XNode)

  module XCommit = struct
    module Val = struct
      type t = G.Value.Commit.t
      type hash = H.t [@@deriving irmin]

      let info_of_git author message =
        let id = author.Git.User.name in
        let date, _ = author.Git.User.date in
        (* FIXME: tz offset is ignored *)
        Irmin.Info.v ~date ~author:id message

      let name_email name =
        let name = String.trim name in
        try
          let i = String.rindex name ' ' in
          let email = String.sub name (i + 1) (String.length name - i - 1) in
          if
            String.length email > 0
            && email.[0] = '<'
            && email.[String.length email - 1] = '>'
          then
            let email = String.sub email 1 (String.length email - 2) in
            let name = String.trim (String.sub name 0 i) in
            (name, email)
          else (name, "irmin@openmirage.org")
        with Not_found -> (name, "irmin@openmirage.org")

      let of_git g =
        let node = G.Value.Commit.tree g in
        let parents = G.Value.Commit.parents g in
        let author = G.Value.Commit.author g in
        let message = G.Value.Commit.message g in
        let message = Option.value ~default:"" message in
        let info = info_of_git author message in
        (info, node, parents)

      let to_git info node parents =
        let tree = node in
        let parents = List.fast_sort G.Hash.compare parents in
        let author =
          let date = Irmin.Info.date info in
          let name, email = name_email (Irmin.Info.author info) in
          Git.User.{ name; email; date = (date, None) }
        in
        let message = Irmin.Info.message info in
        G.Value.Commit.make (* FIXME: should be v *) ~tree ~parents ~author
          ~committer:author
          (if message = "" then None else Some message)

      let v ~info ~node ~parents = to_git info node parents
      let xnode g = G.Value.Commit.tree g
      let node t = xnode t
      let parents g = G.Value.Commit.parents g

      let info g =
        let author = G.Value.Commit.author g in
        let message = Option.value ~default:"" (G.Value.Commit.message g) in
        info_of_git author message

      module C = Irmin.Private.Commit.Make (H)

      let of_c c = to_git (C.info c) (C.node c) (C.parents c)

      let to_c t =
        let info, node, parents = of_git t in
        C.v ~info ~node ~parents

      let to_bin t = Raw.to_raw (G.Value.commit t)

      let encode_bin =
        Irmin.Type.stage @@ fun (t : t) k ->
        Log.debug (fun l -> l "Commit.encode_bin");
        k (to_bin t)

      let decode_bin =
        Irmin.Type.stage @@ fun buf off ->
        Log.debug (fun l -> l "Commit.decode_bin");
        match Raw.of_raw_with_header ~off buf with
        | Ok (Git.Value.Commit t) -> (String.length buf, t)
        | Ok _ -> failwith "wrong object kind"
        | Error _ -> failwith "wrong object kind"

      let size_of = Irmin.Type.Size.custom_dynamic ()

      let t =
        Irmin.Type.map ~bin:(encode_bin, decode_bin, size_of) C.t of_c to_c
    end

    module Key = H

    include Content_addressable (struct
      type t = Val.t

      let type_eq = function `Commit -> true | _ -> false
      let of_git = function Git.Value.Commit c -> Some c | _ -> None
      let to_git c = G.Value.commit c
    end)
  end

  module Commit = Irmin.Private.Commit.Store (Node) (XCommit)
end

module type BRANCH = sig
  include Irmin.Branch.S

  val pp_ref : t Fmt.t
  val of_ref : string -> (t, [ `Msg of string ]) result
end

module Branch (B : Irmin.Branch.S) : BRANCH with type t = B.t = struct
  open Astring
  include B

  let pp = Irmin.Type.pp B.t
  let pp_ref ppf b = Fmt.pf ppf "refs/heads/%a" pp b

  let of_ref str =
    match String.cuts ~sep:"/" str with
    | "refs" :: "heads" :: b ->
        Irmin.Type.of_string B.t (String.concat ~sep:"/" b)
    | _ -> Error (`Msg (Fmt.strf "%s is not a valid branch" str))
end

module type ATOMIC_WRITE_STORE = functor (G : Git.S) (B : BRANCH) -> sig
  module Key : BRANCH with type t = B.t
  module Val : Irmin.Hash.S with type t = G.Hash.t
  module W : Irmin.Private.Watch.S with type key = Key.t and type value = Val.t

  include
    Irmin.ATOMIC_WRITE_STORE with type key = Key.t and type value = W.value

  val v :
    ?lock:Lwt_mutex.t ->
    head:G.Reference.t option ->
    bare:bool ->
    G.t ->
    t Lwt.t
end

module Irmin_branch_store : ATOMIC_WRITE_STORE =
functor
  (G : Git.S)
  (B : BRANCH)
  ->
  struct
    module Key = B
    module Val = Irmin.Hash.Make (G.Hash)
    module W = Irmin.Private.Watch.Make (Key) (Val)

    let handle_git_err = function
      | Ok x -> Lwt.return x
      | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e

    type t = {
      bare : bool;
      dot_git : Fpath.t;
      git_head : G.Hash.t Git.Reference.contents;
      t : G.t;
      w : W.t;
      m : Lwt_mutex.t;
    }

    let watches = Hashtbl.create 10

    type key = Key.t
    type value = Val.t
    type watch = W.watch * (unit -> unit Lwt.t)

    let branch_of_git r =
      let str = String.trim @@ Git.Reference.to_string r in
      match B.of_ref str with Ok r -> Some r | Error (`Msg _) -> None

    let git_of_branch r = Git.Reference.v (Fmt.to_to_string B.pp_ref r)
    let pp_key = Irmin.Type.pp Key.t

    let mem { t; _ } r =
      Log.debug (fun l -> l "mem %a" pp_key r);
      G.Ref.mem t (git_of_branch r)

    let find { t; _ } r =
      Log.debug (fun l -> l "find %a" pp_key r);
      G.Ref.resolve t (git_of_branch r) >>= function
      | Error (`Reference_not_found _) -> Lwt.return_none
      | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
      | Ok k -> Lwt.return_some k

    let listen_dir t =
      let ( / ) = Filename.concat in
      if G.has_global_watches then
        let dir = Fpath.(to_string @@ (t.dot_git / "refs")) in
        let key file =
          match B.of_ref ("refs" / file) with
          | Ok x -> Some x
          | Error (`Msg e) ->
              Log.err (fun l -> l "listen: file %s: %s" file e);
              None
        in
        W.listen_dir t.w dir ~key ~value:(find t)
      else Lwt.return (fun () -> Lwt.return_unit)

    let watch_key t key ?init f =
      Log.debug (fun l -> l "watch_key %a" pp_key key);
      let* stop = listen_dir t in
      let+ w = W.watch_key t.w key ?init f in
      (w, stop)

    let watch t ?init f =
      Log.debug (fun l -> l "watch");
      let* stop = listen_dir t in
      let+ w = W.watch t.w ?init f in
      (w, stop)

    let unwatch t (w, stop) = stop () >>= fun () -> W.unwatch t.w w

    let v ?lock ~head ~bare t =
      let m = match lock with None -> Lwt_mutex.create () | Some l -> l in
      let dot_git = G.dotgit t in
      let write_head head =
        let head = Git.Reference.Ref head in
        let+ () =
          (if G.has_global_checkout then
           Lwt_mutex.with_lock m (fun () ->
               G.Ref.write t Git.Reference.head head)
          else Lwt.return (Ok ()))
          >|= function
          | Error e ->
              Log.err (fun l -> l "Cannot create HEAD: %a" G.pp_error e)
          | Ok () -> ()
        in
        head
      in
      let+ git_head =
        match head with
        | Some h -> write_head h
        | None -> (
            G.Ref.read t Git.Reference.head >>= function
            | Error (`Reference_not_found _ | `Not_found _) ->
                write_head (git_of_branch B.master)
            | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
            | Ok r -> Lwt.return r)
      in
      let w =
        try Hashtbl.find watches (G.dotgit t)
        with Not_found ->
          let w = W.v () in
          Hashtbl.add watches (G.dotgit t) w;
          w
      in
      { git_head; bare; t; w; dot_git; m }

    let list { t; _ } =
      Log.debug (fun l -> l "list");
      let+ refs = G.Ref.list t in
      List.fold_left
        (fun acc (r, _) ->
          match branch_of_git r with None -> acc | Some r -> r :: acc)
        [] refs

    let write_index t gr gk =
      Log.debug (fun l -> l "write_index");
      if G.has_global_checkout then Log.debug (fun f -> f "write_index");
      let git_head = Git.Reference.Ref gr in
      Log.debug (fun f ->
          f "write_index/if bare=%b head=%a" t.bare Git.Reference.pp gr);
      if (not t.bare) && git_head = t.git_head then (
        Log.debug (fun f -> f "write cache (%a)" Git.Reference.pp gr);

        (* FIXME G.write_index t.t gk *)
        let _ = gk in
        Lwt.return_unit)
      else Lwt.return_unit

    let pp_branch = Irmin.Type.pp B.t

    let set t r k =
      Log.debug (fun f -> f "set %a" pp_branch r);
      let gr = git_of_branch r in
      Lwt_mutex.with_lock t.m @@ fun () ->
      let* () = G.Ref.write t.t gr (Git.Reference.Uid k) >>= handle_git_err in
      let* () = W.notify t.w r (Some k) in
      write_index t gr k

    let remove t r =
      Log.debug (fun f -> f "remove %a" pp_branch r);
      Lwt_mutex.with_lock t.m @@ fun () ->
      G.Ref.remove t.t (git_of_branch r) >>= handle_git_err >>= fun () ->
      W.notify t.w r None

    let eq_head_contents_opt x y =
      match (x, y) with
      | None, None -> true
      | Some x, Some y -> Git.Reference.equal_contents ~equal:G.Hash.equal x y
      | _ -> false

    let test_and_set t r ~test ~set =
      Log.debug (fun f ->
          let pp = Fmt.option ~none:(Fmt.any "<none>") (Irmin.Type.pp Val.t) in
          f "test_and_set %a: %a => %a" pp_branch r pp test pp set);
      let gr = git_of_branch r in
      let c = function None -> None | Some h -> Some (Git.Reference.Uid h) in
      let ok r = handle_git_err r >|= fun () -> true in
      Lwt_mutex.with_lock t.m (fun () ->
          let* x =
            G.Ref.read t.t gr >>= function
            | Error (`Reference_not_found _ | `Not_found _) -> Lwt.return_none
            | Ok x -> Lwt.return_some x
            | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
          in
          let* b =
            if not (eq_head_contents_opt x (c test)) then Lwt.return_false
            else
              match c set with
              | None -> G.Ref.remove t.t gr >>= ok
              | Some h -> G.Ref.write t.t gr h >>= ok
          in
          let* () =
            if
              (* We do not protect [write_index] because it can take a long
                 time and we don't want to hold the lock for too long. Would
                 be safer to grab a lock, although the expanded filesystem
                 is not critical for Irmin consistency (it's only a
                 convenience for the user). *)
              b
            then W.notify t.w r set
            else Lwt.return_unit
          in
          let+ () =
            if b then
              match set with
              | None -> Lwt.return_unit
              | Some v -> write_index t gr v
            else Lwt.return_unit
          in
          b)

    let close _ = Lwt.return_unit

    let clear t =
      Log.debug (fun l -> l "clear");
      Lwt_mutex.with_lock t.m (fun () ->
          let* refs = G.Ref.list t.t in
          Lwt_list.iter_p
            (fun (r, _) ->
              G.Ref.remove t.t r >>= handle_git_err >>= fun () ->
              match branch_of_git r with
              | Some k -> W.notify t.w k None
              | None -> Lwt.return_unit)
            refs)
  end

module Irmin_sync_store
    (G : Git.S)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t)
    (B : Irmin.Branch.S) =
struct
  let src = Logs.Src.create "irmin.git-output" ~doc:"Git output"

  module Gitlog = (val Logs.src_log src : Logs.LOG)
  module H = Irmin.Hash.Make (G.Hash)

  type t = G.t
  type commit = H.t
  type branch = B.t
  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  let git_of_branch_str str = Git.Reference.v ("refs/heads/" ^ str)
  let git_of_branch r = git_of_branch_str (Irmin.Type.to_string B.t r)

  (* let o_head_of_git = function None -> Ok None | Some k -> Ok (Some k) *)

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> Lwt.return (Error err)

  let msgf fmt = Fmt.kstrf (fun err -> `Msg err) fmt
  let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

  let fetch t ?depth (ctx, e) br =
    Log.debug (fun f -> f "fetch %a" Smart_git.Endpoint.pp e);
    let push_stdout msg = Gitlog.info (fun f -> f "%s" msg)
    and push_stderr msg = Gitlog.warn (fun f -> f "%s" msg)
    and deepen =
      match depth with Some depth -> Some (`Depth depth) | None -> None
    and reference = git_of_branch br
    and capabilities =
      [
        `Side_band_64k;
        `Multi_ack_detailed;
        `Ofs_delta;
        `Thin_pack;
        `Report_status;
      ]
    in
    S.fetch ~push_stdout ~push_stderr ~capabilities ~ctx e t ?deepen
      (`Some [ (reference, reference) ])
    >>= function
    | Error `Not_found -> Lwt.return (Error (`Msg "not found"))
    | Error (`Msg err) -> Lwt.return (Error (`Msg err))
    | Error (`Exn err) -> Lwt.return (Error (`Msg (Printexc.to_string err)))
    | Error err ->
        Fmt.kstrf (fun e -> Lwt.return (Error (`Msg e))) "%a" S.pp_error err
    | Ok None -> Lwt.return (Ok None)
    | Ok (Some (_, [ (reference, hash) ])) ->
        let value = Git.Reference.uid hash in
        let br =
          Git.Reference.v ("refs/remotes/origin/" ^ Irmin.Type.to_string B.t br)
        in
        G.Ref.write t br value >|= reword_error (msgf "%a" G.pp_error)
        >>? fun () ->
        G.Ref.write t reference value >|= reword_error (msgf "%a" G.pp_error)
        >>? fun () -> Lwt.return (Ok (Some hash))
    | _ -> assert false

  let push t ?depth:_ (ctx, e) br =
    Log.debug (fun f -> f "push %a" Smart_git.Endpoint.pp e);
    let reference = git_of_branch br in
    let capabilities =
      [
        `Side_band_64k;
        `Multi_ack_detailed;
        `Ofs_delta;
        `Thin_pack;
        `Report_status;
      ]
    in
    S.push ~capabilities ~ctx e t [ `Update (reference, reference) ]
    >|= function
    | Error (`Msg err) -> Error (`Msg err)
    | Error (`Exn exn) -> Error (`Msg (Printexc.to_string exn))
    | Error `Not_found -> Error (`Msg "not found")
    | Error err -> Error (`Msg (Fmt.strf "%a" S.pp_error err))
    | Ok () -> Ok ()
end

type reference =
  [ `Branch of string | `Remote of string | `Tag of string | `Other of string ]

module Reference : BRANCH with type t = reference = struct
  open Astring

  type t =
    [ `Branch of string | `Remote of string | `Tag of string | `Other of string ]
  [@@deriving irmin]

  let pp_ref ppf = function
    | `Branch b -> Fmt.pf ppf "refs/heads/%s" b
    | `Remote r -> Fmt.pf ppf "refs/remotes/%s" r
    | `Tag t -> Fmt.pf ppf "refs/tags/%s" t
    | `Other o -> Fmt.pf ppf "refs/%s" o

  let path l = String.concat ~sep:"/" l

  let of_ref str =
    match String.cuts ~sep:"/" str with
    | "refs" :: "heads" :: b -> Ok (`Branch (path b))
    | "refs" :: "remotes" :: r -> Ok (`Remote (path r))
    | "refs" :: "tags" :: t -> Ok (`Tag (path t))
    | "refs" :: o -> Ok (`Other (path o))
    | _ -> Error (`Msg (Fmt.strf "%s is not a valid reference" str))

  let t = Irmin.Type.like t ~pp:pp_ref ~of_string:of_ref
  let master = `Branch Irmin.Branch.String.master

  let is_valid = function
    | `Branch s | `Tag s | `Remote s | `Other s ->
        Irmin.Branch.String.is_valid s
end

module type S = sig
  module Git : Git.S
  include Irmin.S with type metadata = Metadata.t and type hash = Git.Hash.t

  val git_commit : Repo.t -> commit -> Git.Value.Commit.t option Lwt.t
  val git_of_repo : Repo.t -> Git.t

  val repo_of_git :
    ?head:Git.Reference.t ->
    ?bare:bool ->
    ?lock:Lwt_mutex.t ->
    Git.t ->
    Repo.t Lwt.t
end

module type G = sig
  include Git.S

  val v : ?dotgit:Fpath.t -> Fpath.t -> (t, error) result Lwt.t
end

module AW_check_closed (AW : ATOMIC_WRITE_STORE) : ATOMIC_WRITE_STORE =
functor
  (G : Git.S)
  (B : BRANCH)
  ->
  struct
    module Key = B
    module Val = Irmin.Hash.Make (G.Hash)
    module W = Irmin.Private.Watch.Make (Key) (Val)
    module S = AW (G) (B)

    type t = { closed : bool ref; t : S.t }
    type key = S.key
    type value = S.value

    let check_not_closed t = if !(t.closed) then raise Irmin.Closed

    let mem t k =
      check_not_closed t;
      S.mem t.t k

    let find t k =
      check_not_closed t;
      S.find t.t k

    let set t k v =
      check_not_closed t;
      S.set t.t k v

    let test_and_set t k ~test ~set =
      check_not_closed t;
      S.test_and_set t.t k ~test ~set

    let remove t k =
      check_not_closed t;
      S.remove t.t k

    let list t =
      check_not_closed t;
      S.list t.t

    type watch = S.watch

    let watch t ?init f =
      check_not_closed t;
      S.watch t.t ?init f

    let watch_key t k ?init f =
      check_not_closed t;
      S.watch_key t.t k ?init f

    let unwatch t w =
      check_not_closed t;
      S.unwatch t.t w

    let v ?lock ~head ~bare t =
      let+ t = S.v ?lock ~head ~bare t in
      { closed = ref false; t }

    let close t =
      if !(t.closed) then Lwt.return_unit
      else (
        t.closed := true;
        S.close t.t)

    let clear t =
      check_not_closed t;
      S.clear t.t
  end

module Make_ext
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : BRANCH) =
struct
  module R = AW_check_closed (Irmin_branch_store) (G) (B)

  type r = { config : Irmin.config; closed : bool ref; g : G.t; b : R.t }

  module P = struct
    module Hash = Irmin.Hash.Make (G.Hash)

    module XSync = struct
      include Irmin_sync_store (G) (S) (R.Key)

      let v repo = Lwt.return repo.g
    end

    include Make_private (G) (C) (P)
    module Branch = R
    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = XSync

    module Repo = struct
      type t = r

      let branch_t t = t.b
      let contents_t t : 'a Contents.t = (t.closed, t.g)
      let node_t t : 'a Node.t = (contents_t t, (t.closed, t.g))
      let commit_t t : 'a Commit.t = (node_t t, (t.closed, t.g))
      let batch t f = f (contents_t t) (node_t t) (commit_t t)

      type config = {
        root : string;
        dot_git : string option;
        level : int option;
        buffers : int option;
        head : G.Reference.t option;
        bare : bool;
      }

      let config c =
        let root =
          match Irmin.Private.Conf.get c Conf.root with
          | None -> "."
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
        let { root; dot_git; head; bare; _ } = config conf in
        let dotgit = fopt Fpath.v dot_git in
        let root = Fpath.v root in
        let* g = G.v ?dotgit root >>= handle_git_err in
        let+ b = R.v ~head ~bare g in
        { g; b; closed = ref false; config = conf }

      let close t = R.close t.b >|= fun () -> t.closed := true
    end
  end

  include Irmin.Of_private (P)

  let git_commit (repo : Repo.t) (h : commit) : G.Value.Commit.t option Lwt.t =
    let h = Commit.hash h in
    G.read repo.g h >|= function Ok (Git.Value.Commit c) -> Some c | _ -> None

  let git_of_repo r = r.g

  let repo_of_git ?head ?(bare = true) ?lock g =
    let+ b = R.v ?lock ~head ~bare g in
    { config = Irmin.Private.Conf.empty; closed = ref false; g; b }

  module Git = G
end

module Mem = struct
  include Git.Mem.Store

  let confs = Hashtbl.create 10
  let find_conf c = Hashtbl.find_opt confs c

  let add_conf c t =
    Hashtbl.replace confs c t;
    t

  let v' ?dotgit root = v ?dotgit root

  let v ?dotgit root =
    let conf = (dotgit, root) in
    match find_conf conf with
    | Some x -> Lwt.return x
    | None -> v' ?dotgit root >|= add_conf conf
end

module Make
    (G : G)
    (S : Git.Sync.S with type hash = G.hash and type store = G.t)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) =
  Make_ext (G) (S) (C) (P) (Branch (B))

module No_sync (G : Git.S) = struct
  type hash = G.hash
  type store = G.t

  type error =
    [ `Not_found | `Msg of string | `Exn of exn | `Cycle | `Invalid_flow ]

  let pp_error _ _ = assert false

  let fetch ?push_stdout:_ ?push_stderr:_ ?threads:_ ~ctx:_ _ _ ?version:_
      ?capabilities:_ ?deepen:_ _ =
    assert false

  let push ~ctx:_ _ _ ?version:_ ?capabilities:_ _ = assert false
end

module Content_addressable (G : Git.S) (V : Irmin.Type.S) = struct
  module G = struct
    include G

    let v ?dotgit:_ _root = assert false
  end

  module V = struct
    include V

    let merge = Irmin.Merge.default Irmin.Type.(option V.t)
  end

  module M = Make_ext (G) (No_sync (G)) (V) (Irmin.Path.String_list) (Reference)
  module X = M.Private.Contents

  let state t =
    let+ r = M.repo_of_git (snd t) in
    M.Private.Repo.contents_t r

  type 'a t = bool ref * G.t
  type key = X.key
  type value = X.value

  let with_state f t x =
    let* t = state t in
    f t x

  let add = with_state X.add
  let pp_key = Irmin.Type.pp X.Key.t
  let equal_key = Irmin.Type.(unstage (equal X.Key.t))

  let unsafe_add t k v =
    let+ k' = with_state X.add t v in
    if equal_key k k' then ()
    else
      Fmt.failwith
        "[Git.unsafe_append] %a is not a valid key. Expecting %a instead.\n"
        pp_key k pp_key k'

  let find = with_state X.find
  let mem = with_state X.mem
  let clear _ = Lwt.fail_with "not implemented"
end

module Atomic_write (G : Git.S) (K : Irmin.Branch.S) = struct
  module K = struct
    include K

    let master =
      match Irmin.Type.of_string K.t "master" with
      | Ok x -> x
      | Error (`Msg e) -> failwith e
  end

  include AW_check_closed (Irmin_branch_store) (G) (Branch (K))
end

module KV
    (G : G)
    (S : Git.Sync.S with type hash = G.hash and type store = G.t)
    (C : Irmin.Contents.S) =
  Make (G) (S) (C) (Irmin.Path.String_list) (Irmin.Branch.String)

module Ref
    (G : G)
    (S : Git.Sync.S with type hash = G.hash and type store = G.t)
    (C : Irmin.Contents.S) =
  Make_ext (G) (S) (C) (Irmin.Path.String_list) (Reference)

module type S_MAKER = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  ->
  S
    with type key = P.t
     and type step = P.step
     and module Key = P
     and type contents = C.t
     and type branch = B.t
     and module Git = G
     and type Private.Sync.endpoint = Mimic.ctx * Smart_git.Endpoint.t

module type KV_MAKER = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = string
     and module Git = G
     and type Private.Sync.endpoint = Mimic.ctx * Smart_git.Endpoint.t

module type REF_MAKER = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = reference
     and module Git = G
     and type Private.Sync.endpoint = Mimic.ctx * Smart_git.Endpoint.t

include Conf

module Generic
    (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : Irmin.ATOMIC_WRITE_STORE_MAKER)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) =
struct
  (* We use a dummy store to get the serialisation functions. This is
     probably not necessary and we could use Git.Value.Raw instead. *)
  module G = Mem
  module S = Make (G) (No_sync (G)) (C) (P) (B)

  include
    Irmin.Make_ext (CA) (AW) (S.Private.Node.Metadata) (S.Private.Contents.Val)
      (S.Private.Node.Path)
      (S.Branch)
      (S.Private.Hash)
      (S.Private.Node.Val)
      (S.Private.Commit.Val)
end

module Generic_KV
    (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : Irmin.ATOMIC_WRITE_STORE_MAKER)
    (C : Irmin.Contents.S) =
  Generic (CA) (AW) (C) (Irmin.Path.String_list) (Irmin.Branch.String)
