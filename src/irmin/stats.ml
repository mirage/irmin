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

module Make (P: S.PRIVATE) = struct

  open Lwt.Infix

  module AO (M: sig
      include S.AO
      val store: string
      module Val: S.S0 with type t = value
    end) = struct

    let g =
      let title = Fmt.strf "Irmin: %s store (AO)" M.store in
      Metrics.Graph.v ~title ~ylabel:"I/O" ~yunit:"Bytes" ()

    module X = struct

      open Metrics

      let tags = Tags.[
          string "id";
          string "store";
        ]

      let mem =
        let data _ = Data.v [uint "mem" ~graph:g 1] in
        Src.v "AO.mem" ~tags ~data ~duration:true ~status:true

      let len f = function
        | Error _ -> 0
        | Ok v    ->
          match f v with
          | None   -> 0
          | Some v -> Cstruct.len (Type.encode_cstruct M.Val.t v)

      let lenp = len (fun (_, v) -> Some v)
      let leno = len (fun x -> x)

      let find =
        let data v = Data.v [uint "find" (leno v) ~graph:g ~unit:"Bytes"] in
        Src.v "AO.find" ~tags ~data ~duration:true ~status:true

      let add =
        let data v = Data.v [uint "add" (lenp v) ~graph:g ~unit:"Bytes"] in
        Src.v "AO.add" ~tags ~data ~duration:true ~status:true

    end

    type t = { v : M.t; id: string }
    type key = M.key
    type value = M.value

    let tag t f = f t.id M.store
    let v ~id v = { v; id }

    let mem t k =
      Metrics.run X.mem (tag t) (fun () -> M.mem t.v k)

    let find t k =
      Metrics_lwt.run X.find (tag t) (fun () -> M.find t.v k)

    let add t v =
      Metrics_lwt.run X.add (tag t) (fun () -> M.add t.v v >|= fun k -> (k, v))
      >|= fun (k, _) -> k

  end

  module RW (M: sig
      include S.RW
      module Val: S.S0 with type t = value
    end): sig
    include S.RW with type key = M.key and type value = M.value
    val v: id:string -> M.t -> t
  end = struct

    let g =
      let title = "Irmin: reference store (RW)" in
      Metrics.Graph.v ~title ~ylabel:"I/O" ~yunit:"Bytes" ()

    module X = struct

      open Metrics

      let tags = Tags.[
          string "id";
        ]

      let len f = function
        | Error _ -> 0
        | Ok v    ->
          match f v with
          | None   -> 0
          | Some v -> Cstruct.len (Type.encode_cstruct M.Val.t v)

      let lenop = len (fun (_, v) -> v)
      let leno = len (fun x -> x)
      let len = len (fun x -> Some x)

      let lenl = function
        | Ok l    -> List.length l
        | Error _ -> 0

      let mem =
        let data _ = Data.v [uint "mem" ~graph:g 1] in
        Src.v "RW.mem" ~tags ~data ~duration:true ~status:true

      let find =
        let data v = Data.v [uint "find" (leno v) ~graph:g ~unit:"Bytes"] in
        Src.v "RW.find" ~tags ~data ~duration:true ~status:true

      let set =
        let data v = Data.v [uint "set" (len v) ~graph:g ~unit:"Bytes"] in
        Src.v "RW.test" ~tags ~data ~duration:true ~status:true

      let test_and_set =
        let data v =
          Data.v [uint "test-and-set" (lenop v) ~graph:g  ~unit:"Bytes"]
        in
        Src.v "RW.test_and_set" ~tags ~data ~duration:true ~status:true

      let remove =
        let data _ = Data.v [uint "remove" ~graph:g 1] in
        Src.v "RW.remove_set" ~tags ~data ~duration:true ~status:true

      let list =
        let data v =
          Data.v [uint "children" (lenl v) ~graph:g ~unit:"Number of children"]
        in
        Src.v "RW.list" ~tags ~data ~duration:true ~status:true

      let watch =
        let data n =
          Data.v [uint "watch" n ~graph:g ~unit:"Number of active watchers"]
        in
        Src.v "RW.watch" ~tags ~data

    end

    type t = { v: M.t; id: string; mutable watches: int }
    type key = M.key
    type value = M.value
    type watch = M.watch

    let tag t f = f t.id
    let v ~id v = { v; id; watches = 0 }

    let mem t k =
      Metrics_lwt.run X.mem (tag t) (fun () -> M.mem t.v k)

    let find t k =
      Metrics_lwt.run X.find (tag t) (fun () -> M.find t.v k)

    let set t k v =
      Metrics_lwt.run X.set (tag t) (fun () -> M.set t.v k v >|= fun () -> v)
      >|= fun _ -> ()

    let test_and_set t k ~test ~set =
      Metrics_lwt.run X.test_and_set (tag t)
        (fun () -> M.test_and_set t.v k ~test ~set >|= fun b -> (b, set))
      >|= fun (b, _) -> b

    let remove t k =
      Metrics_lwt.run X.remove (tag t) (fun () -> M.remove t.v k)

    let list t =
      Metrics_lwt.run X.list (tag t) (fun () -> M.list t.v)

    let watch t ?init f =
      t.watches <- t.watches + 1;
      Metrics.add X.watch (tag t) (fun f -> f t.watches);
      M.watch t.v ?init f

    let watch_key t k ?init f =
      t.watches <- t.watches + 1;
      Metrics.add X.watch (tag t) (fun f -> f t.watches);
      M.watch_key t.v k ?init f

    let unwatch t w =
      t.watches <- t.watches - 1;
      Metrics.add X.watch (tag t) (fun f -> f t.watches);
      M.unwatch t.v w

  end

  module Contents = struct
    module Key = P.Contents.Key
    module Val = P.Contents.Val
    include AO(struct
        include P.Contents
        let store = "contents"
      end)
    let merge t = P.Contents.merge t.v
  end

  module Node = struct
    module Key = P.Node.Key
    module Val = P.Node.Val
    module Contents = P.Node.Contents
    module Metadata = P.Node.Metadata
    module Path = P.Node.Path
    include AO(struct
        include P.Node
        let store = "node"
      end)
    let merge t = P.Node.merge t.v
  end

  module Commit = struct
    module Key = P.Commit.Key
    module Val = P.Commit.Val
    module Node = P.Commit.Node
    include AO(struct
        include P.Commit
        let store = "commit"
      end)
    let merge t = P.Commit.merge t.v
  end

  module Branch = struct
    module Key = P.Branch.Key
    module Val = P.Branch.Val
    include RW(P.Branch)
  end

  module Slice = P.Slice

  (* FIXME: add metrics for push/pull/fetch *)
  module Sync = P.Sync

  module Repo = struct
    type t = P.Repo.t
    type id = P.Repo.id
    let v = P.Repo.v

    let branch_t id t = Branch.v ~id (P.Repo.branch_t id t)
    let commit_t id t = Commit.v ~id (P.Repo.commit_t id t)
    let node_t id t = Node.v ~id (P.Repo.node_t id t)
    let contents_t id t = Contents.v ~id (P.Repo.contents_t id t)
  end

end
