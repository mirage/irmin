(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Contents = Ir_contents
module Merge = Ir_merge
module Tag = Ir_tag
module Task = Ir_task
module View = Ir_view.Make
module type VIEW = Ir_view.S
module Snapshot = Ir_snapshot.Make
module Dot = Ir_dot.Make
module type S = Ir_s.STORE

module Hash = Ir_hash
module Path = Ir_path
module Make = Ir_s.Make
module Make_ext = Ir_s.Make_ext

module type RO = Ir_ro.STORE
module type AO = Ir_ao.STORE
module type RW = Ir_rw.STORE
module type HRW = Ir_rw.HIERARCHICAL
module type BC = Ir_bc.STORE
module Hum = Ir_hum

type task = Task.t
type config = Ir_conf.t

module type AO_MAKER = Ir_ao.MAKER
module type RW_MAKER = Ir_rw.MAKER
module type BC_MAKER = Ir_bc.MAKER
module type S_MAKER = Ir_s.MAKER

module Private = struct
  module Conf = Ir_conf
  module Node = Ir_node
  module Commit = Ir_commit
  module Slice = Ir_slice
  module Make = Ir_bc.Make
  module Sync = Ir_sync
  module type S = Ir_bc.PRIVATE
  module Watch = Ir_watch
  module Lock = Ir_lock
end

let version = Ir_version.current

module History = Graph.Persistent.Digraph.ConcreteBidirectional(Hash.SHA1)

module Sync = Ir_sync_ext.Make

type remote = Ir_sync_ext.remote

let remote_store (type t) (module M: S with type t = t) (t:t) =
  let module X = (M: Ir_s.STORE with type t = t) in
  Ir_sync_ext.remote_store (module X) t

let remote_uri = Ir_sync_ext.remote_uri

module type BASIC = S with type tag = string and type head = Hash.SHA1.t

module Basic = Ir_s.Default

type ('a, 'b) basic = (module BASIC with type key = 'a and type value = 'b)

type ('a, 'b) contents =
  (module Contents.S with type Path.t = 'a and type t = 'b)

let basic (type k) (type v)
    (module B: S_MAKER)
    (module C: Contents.S with type Path.t = k and type t = v)
  : (k, v) basic
  =
  let module B = Basic(B)(C) in
  (module B)

module type T = S with type tag = string and type head = Hash.SHA1.t

type ('t, 'k, 'v) s =
  (module S with type tag = string
             and type head = Hash.SHA1.t
             and type t = 't
             and type key = 'k
             and type value = 'v)

type ('k, 'v) pack = E: ('t, 'k, 'v) s * 't -> ('k, 'v) pack

type ('a, 'k, 'v) t = {
  read: 'k -> 'v option Lwt.t;
  read_exn: 'k -> 'v Lwt.t;
  mem: 'k -> bool Lwt.t;
  iter: ('k -> unit Lwt.t) -> unit Lwt.t;
  update: 'k -> 'v -> unit Lwt.t;
  remove: 'k -> unit Lwt.t;
  watch: 'k -> 'v option Lwt_stream.t;
  watch_all: unit -> ('k * 'v option) Lwt_stream.t;
  list: 'k -> 'k list Lwt.t;
  remove_rec: 'k -> unit Lwt.t;
  extend: ('a, 'k, 'v) ext;
}

and (_, 'k, 'v) ext =
  | HRW: ([>`HRW], 'k, 'v) ext
  | BC: ('k, 'v) bc -> ([>`BC], 'k, 'v) ext

and ('k, 'v) bc = {
  pack: ('k, 'v) pack;
  tag: unit -> string option Lwt.t;
  tag_exn: unit -> string Lwt.t;
  tags: unit -> string list Lwt.t;
  remove_tag: unit -> unit Lwt.t;
  update_tag: string -> unit Lwt.t;
  merge_tag: ?max_depth:int -> ?n:int -> string -> unit Merge.result Lwt.t;
  merge_tag_exn: ?max_depth:int -> ?n:int -> string -> unit Lwt.t;
  head: unit -> Hash.SHA1.t option Lwt.t;
  head_exn: unit -> Hash.SHA1.t Lwt.t;
  branch: unit -> [`Tag of string | `Head of Hash.SHA1.t];
  heads: unit -> Hash.SHA1.t list Lwt.t;
  update_head: Hash.SHA1.t -> unit Lwt.t;
  compare_and_set_head: test:Hash.SHA1.t option -> set:Hash.SHA1.t option -> bool Lwt.t;
  merge_head: ?max_depth:int -> ?n:int -> Hash.SHA1.t -> unit Merge.result Lwt.t;
  merge_head_exn: ?max_depth:int -> ?n:int -> Hash.SHA1.t -> unit Lwt.t;
  watch_head: 'k -> ('k * Hash.SHA1.t option) Lwt_stream.t;
  watch_tags: unit -> (string * Hash.SHA1.t option) Lwt_stream.t;
  clone: 'm. ('m -> task) -> string -> [`Ok of ('m -> ([`BC], 'k, 'v) t) | `Duplicated_tag] Lwt.t;
  clone_force: 'm. ('m -> task) -> string -> ('m -> ([`BC], 'k, 'v) t) Lwt.t;
  merge: ?max_depth:int -> ?n:int -> into:([`BC], 'k, 'v) t ->
    unit Merge.result Lwt.t;
  merge_exn: ?max_depth:int -> ?n:int -> into:([`BC], 'k, 'v) t -> unit Lwt.t;
  lcas: ?max_depth:int -> ?n:int -> ([`BC], 'k, 'v) t ->
    [`Ok of Hash.SHA1.t list | `Too_many_lcas | `Max_depth_reached] Lwt.t;
  lcas_tag: ?max_depth:int -> ?n:int -> string ->
    [`Ok of Hash.SHA1.t list | `Too_many_lcas | `Max_depth_reached] Lwt.t;
  lcas_head: ?max_depth:int -> ?n:int -> Hash.SHA1.t ->
    [`Ok of Hash.SHA1.t list | `Too_many_lcas | `Max_depth_reached] Lwt.t;
  history: ?depth:int -> ?min:Hash.SHA1.t list -> ?max:Hash.SHA1.t list ->
    unit -> History.t Lwt.t;
  task_of_head: Hash.SHA1.t -> task Lwt.t;
  remote_basic: unit -> remote;
  fetch: ?depth:int -> remote -> Hash.SHA1.t option Lwt.t;
  fetch_exn: ?depth:int -> remote -> Hash.SHA1.t Lwt.t;
  pull: ?depth:int -> remote -> [`Merge | `Update] -> unit Merge.result Lwt.t;
  pull_exn: ?depth:int -> remote -> [`Merge | `Update] -> unit Lwt.t;
  push: ?depth:int -> remote -> [`Ok | `Error] Lwt.t;
  push_exn: ?depth:int -> remote -> unit Lwt.t;
}

(* HRW *)
let read t = t.read
let read_exn t = t.read_exn
let mem t = t.mem
let iter t = t.iter
let update t = t.update
let remove t = t.remove
let watch t = t.watch
let watch_all t = t.watch_all ()
let list t = t.list
let remove_rec t = t.remove_rec

(* BC *)
let bc (t: ([`BC],'k,'v) t) fn = fn t.extend

let tag t = bc t (function BC t -> t.tag ())
let tag_exn t = bc t (function BC t -> t.tag_exn ())
let tags t = bc t (function BC t -> t.tags ())
let remove_tag t = bc t (function BC t -> t.remove_tag ())
let update_tag t = bc t (function BC t -> t.update_tag)
let merge_tag t = bc t (function BC t -> t.merge_tag)
let merge_tag_exn t = bc t (function BC t -> t.merge_tag_exn)
let head t = bc t (function BC t -> t.head ())
let head_exn t = bc t (function BC t -> t.head_exn ())
let branch t = bc t (function BC t -> t.branch ())
let heads t = bc t (function BC t -> t.heads ())
let update_head t = bc t (function BC t -> t.update_head)
let compare_and_set_head t = bc t (function BC t -> t.compare_and_set_head)
let merge_head t = bc t (function BC t -> t.merge_head)
let merge_head_exn t = bc t (function BC t -> t.merge_head_exn)
let watch_head t = bc t (function BC t -> t.watch_head)
let watch_tags t = bc t (function BC t -> t.watch_tags ())
let clone task t = bc t (function BC t -> t.clone task)
let clone_force task t = bc t (function BC t -> t.clone_force task)
let merge a ?max_depth ?n t ~into =
  bc (t a) (function BC t -> t.merge ?max_depth ?n ~into:(into a))
let merge_exn a ?max_depth ?n t ~into =
  bc (t a) (function BC t -> t.merge_exn ?max_depth ?n ~into:(into a))
let lcas a ?max_depth ?n t1 t2 =
  bc (t1 a) (function BC t1 -> t1.lcas ?max_depth ?n (t2 a))
let lcas_tag t = bc t (function BC t -> t.lcas_tag)
let lcas_head t = bc t (function BC t -> t.lcas_head)
let history ?depth ?min ?max t =
  bc t (function BC t -> t.history ?depth ?min ?max ())
let task_of_head t = bc t (function BC t -> t.task_of_head)

(* sync *)
let remote_basic t = bc t (function BC t -> t.remote_basic ())
let fetch t = bc t (function BC t -> t.fetch)
let fetch_exn t = bc t (function BC t -> t.fetch_exn)
let pull t = bc t (function BC t -> t.pull)
let pull_exn t = bc t (function BC t -> t.pull_exn)
let push t = bc t (function BC t -> t.push)
let push_exn t = bc t (function BC t -> t.push_exn)

let pack_hrw (type x) (type k) (type v)
    (module M: HRW with type t = x
                    and type key = k
                    and type value = v)
    (t:x): ([`HRW], k, v) t =
{
  read = M.read t;
  read_exn = M.read_exn t;
  mem = M.mem t;
  iter = M.iter t;
  update = M.update t;
  remove = M.remove t;
  watch = M.watch t;
  watch_all = (fun () -> M.watch_all t);
  list = M.list t;
  remove_rec = M.remove_rec t;
  extend = HRW;
}

let pack_s (type x) (type k) (type v)
    (module M: BASIC with type t = x and type key = k and type value = v)
    (t:'a -> x): 'a -> ([`BC], k, v) t =
  let module S = Sync(M) in
  let merge t ?max_depth ?n ~(into:([`BC], k, v) t) =
    match M.branch t, into.extend with
    | `Tag tag, BC i -> i.merge_tag  ?max_depth ?n tag
    | `Head h , BC i -> i.merge_head ?max_depth ?n h
  in
  let rec aux t =
    let hrw = pack_hrw (module M) t in
    let bc = {
        pack = E ((module M), t);
        tag = (fun () -> M.tag t);
        tag_exn = (fun () -> M.tag_exn t);
        tags = (fun () -> M.tags t);
        remove_tag = (fun () -> M.remove_tag t);
        update_tag = M.update_tag t;
        merge_tag = M.merge_tag t;
        merge_tag_exn = M.merge_tag_exn t;
        head = (fun () -> M.head t);
        head_exn = (fun () -> M.head_exn t);
        branch = (fun () -> M.branch t);
        heads = (fun () -> M.heads t);
        update_head = M.update_head t;
        compare_and_set_head = M.compare_and_set_head t;
        merge_head = M.merge_head t;
        merge_head_exn = M.merge_head_exn t;
        watch_head = M.watch_head t;
        watch_tags = (fun () -> M.watch_tags t);
        clone = (fun task tag ->
            M.clone task t tag >>= function
            | `Ok x           -> Lwt.return (`Ok (fun a -> aux (x a)))
            | `Duplicated_tag -> Lwt.return `Duplicated_tag
          );
        clone_force = (fun task tag ->
            M.clone_force task t tag >>= fun x ->
            Lwt.return (fun a -> aux (x a))
          );
        merge = merge t;
        merge_exn = (fun ?max_depth ?n ~into ->
            merge t ?max_depth ?n ~into >>= Ir_merge.exn
          );
        lcas = (fun ?max_depth ?n i ->
            let BC i = i.extend in
            match i.branch () with
            | `Tag tag -> M.lcas_tag t ?max_depth ?n tag
            | `Head h  -> M.lcas_head t ?max_depth ?n h
          );
        lcas_tag = M.lcas_tag t;
        lcas_head = M.lcas_head t;
        history = (fun ?depth ?min ?max () ->
            M.history ?depth ?min ?max t >>= fun g ->
            History.empty
            |> M.History.fold_vertex (fun x g' -> History.add_vertex g' x) g
            |> M.History.fold_edges (fun x y g' -> History.add_edge g' x y) g
            |> Lwt.return);
        task_of_head = M.task_of_head t;
        remote_basic = (fun () -> remote_store (module M) t);
        fetch = S.fetch t;
        fetch_exn = S.fetch_exn t;
        pull = S.pull t;
        pull_exn = S.pull_exn t;
        push = S.push t;
        push_exn = S.push_exn t;
    }
    in
    { hrw with extend = BC bc }
  in
  fun a -> aux (t a)

let create (type a) (type b) (t: (a, b) basic) config task =
  let (module T) = t in
  T.create config task >>= fun t ->
  return (pack_s (module T) t)

let of_tag (type a) (type b) (t: (a, b) basic) config task tag =
  let (module T) = t in
  T.of_tag config task tag >>= fun t ->
  return (pack_s (module T) t)

let of_head (type a) (type b) (t: (a, b) basic) config task h =
  let (module T) = t in
  let module View = View(T) in
  T.of_head config task h >>= fun t ->
  return (pack_s (module T) t)

let impl (type a) (type b): ([`BC], a, b) t -> (a, b) basic =
  fun t -> match t.extend with
    | BC { pack = E ((module M), _); _ } -> (module M)

let with_hrw_view (type a) (type b)
    (t :([`BC], a, b) t) ?path strat (ops: ([`HRW], a, b) t -> unit Lwt.t) =
  match t.extend with
  | BC { pack = E ((module M), t); _ } ->
    let module V = View(M) in
    let path = match path with
      | Some p -> p
      | None   -> M.Key.empty
    in
    V.of_path t path >>= fun view ->
    let v = pack_hrw (module V) view in
    ops v >>= fun () ->
    match strat with
    | `Update -> V.update_path t path view >>= fun () -> Merge.OP.ok ()
    | `Rebase -> V.rebase_path t path view
    | `Merge  -> V.merge_path t path view
