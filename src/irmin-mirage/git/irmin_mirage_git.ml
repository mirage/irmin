(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Irmin_mirage_git_intf

let remote ?(ctx = Mimic.empty) ?headers uri =
  let ( ! ) f a b = f b a in
  match Smart_git.Endpoint.of_string uri with
  | Ok edn ->
      let edn =
        Option.fold ~none:edn
          ~some:(!Smart_git.Endpoint.with_headers_if_http edn)
          headers
      in
      (ctx, edn)
  | Error (`Msg err) -> Fmt.invalid_arg "remote: %s" err

module Maker (G : Irmin_git.G) = struct
  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  module Maker = Irmin_git.Maker (G) (Git.Mem.Sync (G))

  module Make
      (S : Irmin_git.Schema.S
             with type Hash.t = G.hash
              and type Node.t = G.Value.Tree.t
              and type Commit.t = G.Value.Commit.t) =
  struct
    include Maker.Make (S)

    let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
  end
end

module Ref (G : Irmin_git.G) = struct
  module Maker = Irmin_git.Ref (G) (Git.Mem.Sync (G))
  module G = G

  type branch = Maker.branch
  type endpoint = Maker.endpoint

  module Make (C : Irmin.Contents.S) = struct
    include Maker.Make (C)

    let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
  end
end

module KV (G : Irmin_git.G) = struct
  module Maker = Irmin_git.KV (G) (Git.Mem.Sync (G))
  module G = G

  type endpoint = Maker.endpoint
  type branch = Maker.branch

  module Make (C : Irmin.Contents.S) = struct
    include Maker.Make (C)

    let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
  end
end

module KV_RO (G : Git.S) = struct
  module Key = Mirage_kv.Key

  type key = Key.t

  module G = struct
    include G

    let v ?dotgit:_ _root = assert false
  end

  module Maker = KV (G)
  module S = Maker.Make (Irmin.Contents.String)
  module Sync = Irmin.Sync.Make (S)

  let disconnect _ = Lwt.return_unit

  type error = [ Mirage_kv.error | S.write_error ]

  let pp_error ppf = function
    | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
    | #S.write_error as e -> Irmin.Type.pp S.write_error_t ppf e

  let err e : ('a, error) result = Error e
  let err_not_found k = err (`Not_found k)

  let path x =
    (* XXX(samoht): we should probably just push the Key module in
         Irmin and remove the path abstraction completely ... *)
    Key.segments x

  module Tree = struct
    type t = { tree : S.tree }

    let digest t key =
      match S.Tree.find_tree t.tree (path key) with
      | None -> err_not_found key
      | Some tree ->
          let h = S.Tree.hash tree in
          Ok (Irmin.Type.to_string S.Hash.t h)

    let list t key =
      let l = S.Tree.list t.tree (path key) in
      let l =
        List.map
          (fun (s, k) ->
            ( Mirage_kv.Key.v s,
              match S.Tree.destruct k with
              | `Contents _ -> `Value
              | `Node _ -> `Dictionary ))
          l
      in
      Ok l

    let exists t key =
      match S.Tree.kind t.tree (path key) with
      | Some `Contents -> Ok (Some `Value)
      | Some `Node -> Ok (Some `Dictionary)
      | None -> Ok None

    let get t key =
      match S.Tree.find t.tree (path key) with
      | None -> err_not_found key
      | Some v -> Ok v

    let get_partial t key ~offset ~length =
      match get t key with
      | Error e -> Error e
      | Ok data ->
          Ok
            (let len = String.length data in
             let off = Optint.Int63.to_int offset in
             if off >= len || off < 0 || length < 0 then ""
             else
               let l = min length (len - off) in
               String.sub data off l)

    let size t key =
      get t key
      |> Result.map (fun data -> Optint.Int63.of_int (String.length data))
  end

  type t = { root : S.path; t : S.t }

  let head_message t =
    match S.Head.find t.t with
    | None -> "empty HEAD"
    | Some h ->
        let info = S.Commit.info h in
        Fmt.str "commit: %a\nAuthor: %s\nDate: %Ld\n\n%s\n" S.Commit.pp_hash h
          (S.Info.author info) (S.Info.date info) (S.Info.message info)

  let last_modified t key =
    let key' = path key in
    match S.last_modified t.t key' with
    | [] -> Error (`Not_found key)
    | h :: _ -> Ok (Ptime.v (0, S.Info.date (S.Commit.info h)))

  let connect ?depth ?(branch = "main") ?(root = Mirage_kv.Key.empty) ?ctx
      ?headers t uri =
    Lwt_eio.run_eio @@ fun () ->
    let remote = S.remote ?ctx ?headers uri in
    let head = Git.Reference.v ("refs/heads/" ^ branch) in
    let repo = S.repo_of_git ~bare:true ~head t in
    let t = S.of_branch repo branch in
    let _ = Sync.pull_exn t ?depth remote `Set in
    let root = path root in
    { t; root }

  let tree t =
    let tree =
      match S.find_tree t.t t.root with
      | None -> S.Tree.empty ()
      | Some tree -> tree
    in
    { Tree.tree }

  let exists t k = Tree.exists (tree t) k
  let get t k = Tree.get (tree t) k

  let get_partial t k ~offset ~length =
    Lwt_eio.run_eio @@ fun () -> Tree.get_partial (tree t) k ~offset ~length

  let list t k = Tree.list (tree t) k
  let digest t k = Tree.digest (tree t) k
  let size t k = Tree.size (tree t) k

  let get t k =
    match Key.segments k with [ "HEAD" ] -> Ok (head_message t) | _ -> get t k

  let exists t k = Lwt_eio.run_eio @@ fun () -> exists t k
  let get t k = Lwt_eio.run_eio @@ fun () -> get t k
  let list t k = Lwt_eio.run_eio @@ fun () -> list t k
  let digest t k = Lwt_eio.run_eio @@ fun () -> digest t k
  let size t k = Lwt_eio.run_eio @@ fun () -> size t k
  let last_modified t k = Lwt_eio.run_eio @@ fun () -> last_modified t k
end

module KV_RW (G : Irmin_git.G) (C : Mirage_clock.PCLOCK) = struct
  (* XXX(samoht): batches are stored in memory. This could be bad if
       large objects are stored for too long... Might be worth having
       a clever LRU, which pushes larges objects to the underlying
       layer when needed. *)

  module RO = KV_RO (G)
  module S = RO.S
  module Tree = RO.Tree
  module Info = Irmin_mirage.Info (S.Info) (C)

  type batch = { mutable tree : S.tree; origin : S.commit }

  type store = Batch of batch | Store of RO.t

  and t = {
    store : store;
    author : unit -> string;
    msg : [ `Set of RO.key | `Remove of RO.key | `Batch ] -> string;
    remote : Irmin.remote;
  }

  type key = RO.key
  type error = RO.error

  let pp_error = RO.pp_error
  let default_author () = "irmin <irmin@mirage.io>"

  let default_msg = function
    | `Set k -> Fmt.str "Updating %a" Mirage_kv.Key.pp k
    | `Remove k -> Fmt.str "Removing %a" Mirage_kv.Key.pp k
    | `Batch -> "Commmiting batch operation"

  let connect ?depth ?branch ?root ?ctx ?headers ?(author = default_author)
      ?(msg = default_msg) git uri =
    let open Lwt.Syntax in
    let+ t = RO.connect ?depth ?branch ?root ?ctx ?headers git uri in
    let remote = S.remote ?ctx ?headers uri in
    { store = Store t; author; msg; remote }

  let disconnect t =
    match t.store with Store t -> RO.disconnect t | Batch _ -> Lwt.return_unit

  (* XXX(samoht): always return the 'last modified' on the
       underlying storage layer, not for the current batch. *)
  let last_modified t key =
    match t.store with
    | Store t -> RO.last_modified t key
    | Batch b ->
        let t = RO.S.of_commit b.origin in
        RO.last_modified { root = S.Path.empty; t } key

  let tree t =
    match t.store with
    | Store t -> RO.tree t
    | Batch b -> { Tree.tree = b.tree }

  let digest t k = Lwt_eio.run_eio @@ fun () -> Tree.digest (tree t) k
  let size t k = Lwt_eio.run_eio @@ fun () -> Tree.size (tree t) k
  let exists t k = Lwt_eio.run_eio @@ fun () -> Tree.exists (tree t) k
  let get t k = Lwt_eio.run_eio @@ fun () -> Tree.get (tree t) k

  let get_partial t k ~offset ~length =
    Lwt_eio.run_eio @@ fun () -> Tree.get_partial (tree t) k ~offset ~length

  let list t k = Lwt_eio.run_eio @@ fun () -> Tree.list (tree t) k

  type write_error = [ RO.error | Mirage_kv.write_error | RO.Sync.push_error ]

  let pp_write_error ppf = function
    | #RO.error as e -> RO.pp_error ppf e
    | #RO.Sync.push_error as e -> RO.Sync.pp_push_error ppf e
    | #Mirage_kv.write_error as e -> Mirage_kv.pp_write_error ppf e

  let info t op = Info.f ~author:(t.author ()) "%s" (t.msg op)
  let path = RO.path

  let write_error = function
    | Ok _ -> Ok ()
    | Error e -> Error (e :> write_error)

  let ( >?= ) r f =
    Lwt.bind r (function
      | Error e -> Lwt.return_error (e :> write_error)
      | Ok r -> f r)

  let set t k v =
    Lwt_eio.run_eio @@ fun () ->
    let info = info t (`Set k) in
    match t.store with
    | Store s -> (
        match S.set ~info s.t (path k) v with
        | Ok _ -> RO.Sync.push s.t t.remote |> write_error
        | Error e -> Error (e :> write_error))
    | Batch b ->
        let tree = S.Tree.add b.tree (path k) v in
        b.tree <- tree;
        Ok ()

  let set_partial t k ~offset v =
    let off = Optint.Int63.to_int offset in
    if off < 0 then Lwt.return_ok ()
    else
      get t k >?= fun data ->
      let data_len = String.length data in
      let v_len = String.length v in
      let buf = Bytes.make (max data_len (off + v_len)) '\000' in
      Bytes.blit_string data 0 buf 0 data_len;
      Bytes.blit_string v 0 buf off v_len;
      set t k (Bytes.unsafe_to_string buf)

  let remove t k =
    Lwt_eio.run_eio @@ fun () ->
    let info = info t (`Remove k) in
    match t.store with
    | Store s -> (
        match S.remove ~info s.t (path k) with
        | Ok _ -> RO.Sync.push s.t t.remote |> write_error
        | Error e -> Error (e :> write_error))
    | Batch b ->
        let tree = S.Tree.remove b.tree (path k) in
        b.tree <- tree;
        Ok ()

  let rename t ~source ~dest =
    get t source >?= fun data ->
    remove t source >?= fun () -> set t dest data

  let allocate t k ?last_modified:_ size =
    let size = Optint.Int63.to_int size in
    if size < 0 then Lwt.return_ok ()
    else
      exists t k >?= function
      | Some _ -> Lwt.return_error (`Already_present k)
      | None -> set t k (String.make size '\000')

  let get_store_tree (t : RO.t) =
    match S.Head.find t.t with
    | None -> None
    | Some origin -> (
        let tree = S.Commit.tree origin in
        match S.Tree.find_tree tree t.root with
        | Some t -> Some (origin, t)
        | None -> Some (origin, S.Tree.empty ()))

  let batch t ?(retries = 42) f =
    let info = info t `Batch in
    let one t =
      match t.store with
      | Batch _ -> Fmt.failwith "No recursive batches"
      | Store s -> (
          (* get the tree origin *)
          match get_store_tree s with
          | None -> Ok (f t) (* no transaction is needed *)
          | Some (origin, old_tree) -> (
              let batch = { tree = old_tree; origin } in
              let b = Batch batch in
              let result = f { t with store = b } in
              match get_store_tree s with
              | None ->
                  (* Someting weird happened, retring *)
                  Error `Retry
              | Some (_, main_tree) -> (
                  match
                    Irmin.Merge.f S.Tree.merge
                      ~old:(Irmin.Merge.promise old_tree)
                      main_tree batch.tree
                  with
                  | Error (`Conflict _) -> Error `Retry
                  | Ok new_tree -> (
                      match S.set_tree s.t ~info s.root new_tree with
                      | Ok () -> Ok result
                      | Error _ -> Error `Retry))))
    in
    let rec loop = function
      | 0 -> failwith "Too many retries"
      | n -> ( match one t with Error `Retry -> loop (n - 1) | Ok r -> r)
    in
    let r = loop retries in
    match t.store with
    | Batch _ -> Fmt.failwith "No recursive batches"
    | Store s -> (
        match RO.Sync.push s.t t.remote with
        | Ok _ -> r
        | Error e -> failwith (Fmt.to_to_string RO.Sync.pp_push_error e))
end

module Mem = struct
  module G = Irmin_git.Mem
  include Maker (G)

  module Maker = struct
    module Ref = Ref (G)
    module KV = KV (G)
  end

  module Ref = Maker.Ref
  module KV = Maker.KV
  module KV_RO = KV_RO (G)
  module KV_RW = KV_RW (G)
end
