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
open Lwt.Infix
include Command_intf

module Make (IO : Conn.IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) =
struct
  module Store = Store
  module Tree = Tree.Make (Store)
  module Commit = Commit.Make (Store) (Tree)
  include Context.Make (IO) (Codec) (Store) (Tree)
  module Return = Conn.Return

  type t = (module CMD)

  module Commands = struct
    module Tree' = Tree
    module Tree = Command_tree.Make (IO) (Codec) (Store) (Tree) (Commit)

    module Ping = struct
      let name = "ping"

      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let run conn _ctx _ () = Return.ok conn
    end

    module Set_current_branch = struct
      type req = Store.Branch.t [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "set_current_branch"

      let run conn ctx _ branch =
        let* store =
          Lwt_eio.run_eio @@ fun () -> Store.of_branch ctx.repo branch
        in
        ctx.branch <- branch;
        ctx.store <- store;
        Return.ok conn
    end

    module Get_current_branch = struct
      type req = unit [@@deriving irmin]
      type res = Store.Branch.t [@@deriving irmin]

      let name = "get_current_branch"
      let run conn ctx _ () = Return.v conn Store.Branch.t ctx.branch
    end

    module Export = struct
      type req = int option [@@deriving irmin]
      type res = Store.slice [@@deriving irmin]

      let name = "export"

      let run conn ctx _ depth =
        let* slice =
          Lwt_eio.run_eio @@ fun () ->
          Store.Repo.export ?depth ~full:true ~max:`Head ctx.repo
        in
        Return.v conn Store.slice_t slice
    end

    module Import = struct
      type req = Store.slice [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "import"

      let run conn ctx _ slice =
        let* () =
          (Lwt_eio.run_eio @@ fun () -> Store.Repo.import ctx.repo slice)
          >|= Error.unwrap "import"
        in
        Return.ok conn
    end

    open Store.Backend

    module Contents = struct
      type key = Contents.key

      let key_t = Contents.Key.t

      type value = Contents.value

      let value_t = Contents.Val.t

      type hash = Contents.hash

      let hash_t = Contents.Hash.t

      module Mem = struct
        let name = "contents.mem"

        type req = key [@@deriving irmin]
        type res = bool [@@deriving irmin]

        let run conn ctx _ key =
          let* x =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun contents _ _ -> Contents.mem contents key)
          in
          Return.v conn res_t x
      end

      module Find = struct
        let name = "contents.find"

        type req = key [@@deriving irmin]
        type res = value option [@@deriving irmin]

        let run conn ctx _ key =
          let* v =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun contents _ _ -> Contents.find contents key)
          in
          Return.v conn res_t v
      end

      module Add = struct
        let name = "contents.add"

        type req = value [@@deriving irmin]
        type res = key [@@deriving irmin]

        let run conn ctx _ value =
          let* k =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun contents _ _ ->
                Contents.add contents value)
          in
          Return.v conn res_t k
      end

      module Unsafe_add = struct
        let name = "contents.unsafe_add"

        type req = hash * value [@@deriving irmin]
        type res = key [@@deriving irmin]

        let run conn ctx _ (hash, value) =
          let* k =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun contents _ _ ->
                Contents.unsafe_add contents hash value)
          in
          Return.v conn res_t k
      end

      module Index = struct
        let name = "contents.index"

        type req = hash [@@deriving irmin]
        type res = key option [@@deriving irmin]

        let run conn ctx _ hash =
          let* v =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun contents _ _ ->
                Contents.index contents hash)
          in
          Return.v conn res_t v
      end

      module Merge = struct
        let name = "contents.merge"

        type req = key option option * key option * key option
        [@@deriving irmin]

        type res = (key option, Irmin.Merge.conflict) Result.t
        [@@deriving irmin]

        let run conn ctx _ (old, a, b) =
          let* res =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun contents _ _ ->
                let merge = Contents.merge contents in
                let f = Irmin.Merge.f merge in
                let old () = Ok old in
                f ~old a b)
          in
          Return.v conn res_t res
      end
    end

    module Node = struct
      type key = Node.key

      let key_t = Node.Key.t

      type value = Node.value

      let value_t = Node.Val.t

      type hash = Hash.t

      module Mem = struct
        let name = "node.mem"

        type req = key [@@deriving irmin]
        type res = bool [@@deriving irmin]

        let run conn ctx _ key =
          let* x =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ node _ -> Node.mem node key)
          in
          Return.v conn res_t x
      end

      module Find = struct
        let name = "node.find"

        type req = key [@@deriving irmin]
        type res = value option [@@deriving irmin]

        let run conn ctx _ key =
          let* v =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ node _ -> Node.find node key)
          in
          Return.v conn res_t v
      end

      module Add = struct
        let name = "node.add"

        type req = value [@@deriving irmin]
        type res = key [@@deriving irmin]

        let run conn ctx _ value =
          let* k =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ node _ -> Node.add node value)
          in
          Return.v conn res_t k
      end

      module Unsafe_add = struct
        let name = "node.unsafe_add"

        type req = Hash.t * value [@@deriving irmin]
        type res = key [@@deriving irmin]

        let run conn ctx _ (hash, value) =
          let* k =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ node _ ->
                Node.unsafe_add node hash value)
          in
          Return.v conn res_t k
      end

      module Index = struct
        let name = "node.index"

        type req = Hash.t [@@deriving irmin]
        type res = key option [@@deriving irmin]

        let run conn ctx _ hash =
          let* v =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ node _ -> Node.index node hash)
          in
          Return.v conn res_t v
      end

      module Merge = struct
        let name = "node.merge"

        type req = key option option * key option * key option
        [@@deriving irmin]

        type res = (key option, Irmin.Merge.conflict) Result.t
        [@@deriving irmin]

        let run conn ctx _ (old, a, b) =
          let* res =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ node _ ->
                let merge = Node.merge node in
                let f = Irmin.Merge.f merge in
                let old () = Ok old in
                f ~old a b)
          in
          Return.v conn res_t res
      end
    end

    module Commit = struct
      type key = Commit.key

      let key_t = Commit.Key.t

      type value = Commit.value

      let value_t = Commit.Val.t

      type hash = Hash.t

      module Mem = struct
        let name = "commit.mem"

        type req = key [@@deriving irmin]
        type res = bool [@@deriving irmin]

        let run conn ctx _ key =
          let x = Repo.commit_t ctx.repo in
          let* v = Lwt_eio.run_eio @@ fun () -> Commit.mem x key in
          Return.v conn res_t v
      end

      module Find = struct
        let name = "commit.find"

        type req = key [@@deriving irmin]
        type res = value option [@@deriving irmin]

        let run conn ctx _ key =
          let x = Repo.commit_t ctx.repo in
          let* v = Lwt_eio.run_eio @@ fun () -> Commit.find x key in
          Return.v conn res_t v
      end

      module Add = struct
        let name = "commit.add"

        type req = value [@@deriving irmin]
        type res = key [@@deriving irmin]

        let run conn ctx _ value =
          let* k =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ _ commit -> Commit.add commit value)
          in
          Return.v conn res_t k
      end

      module Unsafe_add = struct
        let name = "commit.unsafe_add"

        type req = Hash.t * value [@@deriving irmin]
        type res = key [@@deriving irmin]

        let run conn ctx _ (hash, value) =
          let* k =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ _ commit ->
                Commit.unsafe_add commit hash value)
          in
          Return.v conn res_t k
      end

      module Index = struct
        let name = "commit.index"

        type req = Hash.t [@@deriving irmin]
        type res = key option [@@deriving irmin]

        let run conn ctx _ hash =
          let* v =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ _ commit -> Commit.index commit hash)
          in
          Return.v conn res_t v
      end

      module Merge = struct
        let name = "commit.merge"

        type req = Store.Info.t * (key option option * key option * key option)
        [@@deriving irmin]

        type res = (key option, Irmin.Merge.conflict) Result.t
        [@@deriving irmin]

        let run conn ctx _ (info, (old, a, b)) =
          let info () = info in
          let* res =
            Lwt_eio.run_eio @@ fun () ->
            Repo.batch ctx.repo (fun _ _ commit ->
                let merge = Commit.merge commit ~info in
                let f = Irmin.Merge.f merge in
                let old () = Ok old in
                f ~old a b)
          in
          Return.v conn res_t res
      end
    end

    module Branch = struct
      type key = Schema.Branch.t [@@deriving irmin]
      type value = Store.commit_key [@@deriving irmin]

      module Mem = struct
        let name = "branch.mem"

        type req = key [@@deriving irmin]
        type res = bool [@@deriving irmin]

        let run conn ctx _ branch =
          let b = Repo.branch_t ctx.repo in
          let* x = Lwt_eio.run_eio @@ fun () -> Branch.mem b branch in
          Return.v conn res_t x
      end

      module Find = struct
        let name = "branch.find"

        type req = key [@@deriving irmin]
        type res = value option [@@deriving irmin]

        let run conn ctx _ branch =
          let b = Repo.branch_t ctx.repo in
          let* commit = Lwt_eio.run_eio @@ fun () -> Branch.find b branch in
          Return.v conn res_t commit
      end

      module Set = struct
        let name = "branch.set"

        type req = key * value [@@deriving irmin]
        type res = unit [@@deriving irmin]

        let run conn ctx _ (branch, commit) =
          let b = Repo.branch_t ctx.repo in
          let* () = Lwt_eio.run_eio @@ fun () -> Branch.set b branch commit in
          Return.v conn res_t ()
      end

      module Test_and_set = struct
        let name = "branch.test_and_set"

        type req = key * value option * value option [@@deriving irmin]
        type res = bool [@@deriving irmin]

        let run conn ctx _ (branch, test, set) =
          let b = Repo.branch_t ctx.repo in
          let* res =
            Lwt_eio.run_eio @@ fun () -> Branch.test_and_set b branch ~test ~set
          in
          Return.v conn res_t res
      end

      module Remove = struct
        let name = "branch.remove"

        type req = key [@@deriving irmin]
        type res = unit [@@deriving irmin]

        let run conn ctx _ branch =
          let b = Repo.branch_t ctx.repo in
          let* () = Lwt_eio.run_eio @@ fun () -> Branch.remove b branch in
          Return.v conn res_t ()
      end

      module List = struct
        let name = "branch.list"

        type req = unit [@@deriving irmin]
        type res = key list [@@deriving irmin]

        let run conn ctx _ () =
          let b = Repo.branch_t ctx.repo in
          let* b = Lwt_eio.run_eio @@ fun () -> Branch.list b in
          Return.v conn res_t b
      end

      module Clear = struct
        let name = "branch.clear"

        type req = unit [@@deriving irmin]
        type res = unit [@@deriving irmin]

        let run conn ctx _ () =
          let b = Repo.branch_t ctx.repo in
          let* () = Lwt_eio.run_eio @@ fun () -> Branch.clear b in
          Return.v conn res_t ()
      end

      module Watch = struct
        type req = (key * value) list option [@@deriving irmin]
        type res = unit [@@deriving irmin]

        let name = "branch.watch"

        let run conn ctx _ init =
          let b = Repo.branch_t ctx.repo in
          let* () =
            match ctx.branch_watch with
            | Some watch ->
                ctx.branch_watch <- None;

                Lwt_eio.run_eio @@ fun () -> Branch.unwatch b watch
            | None -> Lwt.return_unit
          in
          let* watch =
            Lwt_eio.run_eio @@ fun () ->
            Branch.watch b ?init (fun key diff ->
                Lwt_eio.run_lwt @@ fun () ->
                let diff_t = Irmin.Diff.t Store.commit_key_t in
                Lwt.catch
                  (fun () ->
                    let* () = Conn.Response.write_header conn { status = 0 } in
                    let* () =
                      Conn.write conn
                        (Irmin.Type.pair Store.Branch.t diff_t)
                        (key, diff)
                    in
                    IO.flush conn.oc)
                  (fun _ -> Lwt.return_unit))
          in
          ctx.branch_watch <- Some watch;
          Return.v conn res_t ()
      end

      module Watch_key = struct
        type req = value option * key [@@deriving irmin]
        type res = unit [@@deriving irmin]

        let name = "branch.watch_key"

        let run conn ctx _ (init, key) =
          let b = Repo.branch_t ctx.repo in
          let* () =
            match ctx.branch_watch with
            | Some watch ->
                ctx.branch_watch <- None;
                Lwt_eio.run_eio @@ fun () -> Branch.unwatch b watch
            | None -> Lwt.return_unit
          in
          let* watch =
            Lwt_eio.run_eio @@ fun () ->
            Branch.watch_key b key ?init (fun diff ->
                Lwt_eio.run_lwt @@ fun () ->
                let diff_t = Irmin.Diff.t Store.commit_key_t in
                Lwt.catch
                  (fun () ->
                    let* () = Conn.Response.write_header conn { status = 0 } in
                    let* () = Conn.write conn diff_t diff in
                    IO.flush conn.oc)
                  (fun _ -> Lwt.return_unit))
          in
          ctx.branch_watch <- Some watch;
          Return.v conn res_t ()
      end

      module Unwatch = struct
        type req = unit [@@deriving irmin]
        type res = unit [@@deriving irmin]

        let name = "branch.unwatch"

        let run conn ctx _ () =
          let b = Repo.branch_t ctx.repo in
          let* () =
            match ctx.branch_watch with
            | Some watch ->
                ctx.branch_watch <- None;
                Lwt_eio.run_eio @@ fun () -> Branch.unwatch b watch
            | None -> Lwt.return_unit
          in
          Return.v conn res_t ()
      end
    end

    module Store = struct
      module Mem = struct
        type req = Store.path [@@deriving irmin]
        type res = bool [@@deriving irmin]

        let name = "store.mem"

        let run conn ctx _ path =
          let* res = Lwt_eio.run_eio @@ fun () -> Store.mem ctx.store path in
          Return.v conn res_t res
      end

      module Mem_tree = struct
        type req = Store.path [@@deriving irmin]
        type res = bool [@@deriving irmin]

        let name = "store.mem_tree"

        let run conn ctx _ path =
          let* res =
            Lwt_eio.run_eio @@ fun () -> Store.mem_tree ctx.store path
          in
          Return.v conn res_t res
      end

      module Find = struct
        type req = Store.path [@@deriving irmin]
        type res = Store.contents option [@@deriving irmin]

        let name = "store.find"

        let run conn ctx _ path =
          let* x = Lwt_eio.run_eio @@ fun () -> Store.find ctx.store path in
          Return.v conn res_t x
      end

      module Find_tree = struct
        type req = Store.path [@@deriving irmin]
        type res = Store.Tree.concrete option [@@deriving irmin]

        let name = "store.find_tree"

        let run conn ctx _ path =
          let* x =
            Lwt_eio.run_eio @@ fun () -> Store.find_tree ctx.store path
          in
          match x with
          | None -> Return.v conn res_t None
          | Some x ->
              let* x = Lwt_eio.run_eio @@ fun () -> Store.Tree.to_concrete x in
              Return.v conn res_t (Some x)
      end

      type write_options =
        (bool option * int option) * (bool option * Store.hash list option)
      [@@deriving irmin]

      let mk_parents ctx parents =
        match parents with
        | None -> Lwt.return None
        | Some parents ->
            let* parents =
              Lwt_eio.run_eio @@ fun () ->
              List.filter_map
                (fun hash -> Store.Commit.of_hash ctx.repo hash)
                parents
            in
            Lwt.return_some parents

      module Remove = struct
        type req = write_options * Store.path * Store.Info.t [@@deriving irmin]
        type res = unit [@@deriving irmin]

        let name = "store.remove"

        let run conn ctx _
            (((clear, retries), (allow_empty, parents)), path, info) =
          let* parents = mk_parents ctx parents in
          let* () =
            Lwt_eio.run_eio @@ fun () ->
            Store.remove_exn ?clear ?retries ?allow_empty ?parents ctx.store
              path ~info:(fun () -> info)
          in
          Return.v conn res_t ()
      end
    end
  end

  let commands : (string * (module CMD)) list =
    let open Commands in
    [
      cmd (module Ping);
      cmd (module Set_current_branch);
      cmd (module Get_current_branch);
      cmd (module Import);
      cmd (module Export);
      cmd (module Contents.Mem);
      cmd (module Contents.Find);
      cmd (module Contents.Add);
      cmd (module Contents.Unsafe_add);
      cmd (module Contents.Index);
      cmd (module Contents.Merge);
      cmd (module Node.Mem);
      cmd (module Node.Find);
      cmd (module Node.Add);
      cmd (module Node.Unsafe_add);
      cmd (module Node.Index);
      cmd (module Node.Merge);
      cmd (module Commit.Mem);
      cmd (module Commit.Find);
      cmd (module Commit.Add);
      cmd (module Commit.Unsafe_add);
      cmd (module Commit.Index);
      cmd (module Commit.Merge);
      cmd (module Branch.Mem);
      cmd (module Branch.Find);
      cmd (module Branch.Set);
      cmd (module Branch.Test_and_set);
      cmd (module Branch.Remove);
      cmd (module Branch.List);
      cmd (module Branch.Clear);
      cmd (module Branch.Watch);
      cmd (module Branch.Unwatch);
      cmd (module Branch.Watch_key);
      cmd (module Store.Mem);
      cmd (module Store.Mem_tree);
      cmd (module Store.Find);
      cmd (module Store.Find_tree);
      cmd (module Store.Remove);
    ]
    @ Tree.commands

  let () = List.iter (fun (k, _) -> assert (String.length k < 255)) commands
  let of_name name = List.assoc name commands
  let name (module Cmd : CMD) = Cmd.name
end
