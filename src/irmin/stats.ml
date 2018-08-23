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

    module X = struct

      open Metrics

      let tags = Tags.[
          string "id";
          string "store";
        ]

      let mem =
        let data () = Data.v [int "mem" 1] in
        Src.v "AO.mem" ~tags ~data

      let find =
        let data () = Data.v [int "find" 1] in
        Src.v "AO.find" ~tags ~data

      let add =
        let data n = Data.v [uint "add" n] in
        Src.v "AO.add" ~tags ~data

    end

    type t = { v : M.t; id: string }
    type key = M.key
    type value = M.value

    let tag t f = f t.id M.store
    let v ~id v = { v; id }

    let mem t k =
      Metrics.add X.mem (tag t) (fun l -> l ());
      M.mem t.v k

    let find t k =
      Metrics.add X.find (tag t) (fun l -> l ());
      M.find t.v k

    let add t v =
      Metrics.add X.add (tag t) (fun l ->
          let buf = Type.encode_cstruct M.Val.t v in
          l (Cstruct.len buf)
        );
      M.add t.v v
  end

  module RW (M: sig
      include S.RW
      module Val: S.S0 with type t = value
    end): sig
    include S.RW with type key = M.key and type value = M.value
    val v: id:string -> M.t -> t
  end = struct

    module X = struct

      open Metrics

      let tags = Tags.[
          string "id";
        ]

      let mem =
        let data () = Data.v [int "mem" 1] in
        Src.v "RW.mem" ~tags ~data

      let find =
        let data () = Data.v [int "find" 1] in
        Src.v "RW.find" ~tags ~data

      let set =
        let data n = Data.v [uint "set" n ~unit:"Data written in bytes"] in
        Src.v "RW.test" ~tags ~data

      let test_and_set =
        let data n = Data.v [uint "test-and-set" n ~unit:"Data written in bytes"] in
        Src.v "RW.test_and_set" ~tags ~data

      let remove =
        let data () = Data.v [uint "remove" 1] in
        Src.v "RW.remove" ~tags ~data

      let list =
        let data n = Data.v [uint "children" n ~unit:"Number of children"] in
        Src.v "RW.list" ~tags ~data

      let watch =
        let data n = Data.v [uint "watch" n ~unit:"Number of active watchers"] in
        Src.v "RW.watch" ~tags ~data

    end

    type t = { v: M.t; id: string; mutable watches: int }
    type key = M.key
    type value = M.value
    type watch = M.watch

    let tag t f = f t.id
    let v ~id v = { v; id; watches = 0 }

    let mem t k =
      Metrics.add X.mem (tag t) (fun f -> f ());
      M.mem t.v k

    let find t k =
      Metrics.add X.find (tag t) (fun f -> f ());
      M.find t.v k

    let set t k v =
      Metrics.add X.set (tag t) (fun f ->
          let buf = Type.encode_cstruct M.Val.t v in
          f (Cstruct.len buf)
        );
      M.set t.v k v

    let test_and_set t k ~test ~set =
      Metrics.add X.test_and_set (tag t) (fun f ->
          let len = match set with
            | None   -> 0
            | Some v -> Type.encode_cstruct M.Val.t v |> Cstruct.len
          in
          f len
        );
      M.test_and_set t.v k ~test ~set

    let remove t k =
      Metrics.add X.remove (tag t) (fun f -> f ());
      M.remove t.v k

    let list t =
      M.list t.v >|= fun l ->
      Metrics.add X.list (tag t) (fun f -> f (List.length l));
      l

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
