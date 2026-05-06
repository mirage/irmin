(*
 * Copyright (c) 2026 Tarides
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

(** Adapter functors: take a Lwt-typed backend Maker (Content_addressable or
    Atomic_write), produce the Irmin 4 (Eio-typed) counterpart by bridging each
    operation through {!Lwt_eio.Promise.await_lwt}. Used to feed user-supplied
    Lwt backends into Irmin 4 functors like {!Irmin.Maker}. *)

let await = Lwt_eio.Promise.await_lwt

(* Alias the outer [Schema] module so the local [Schema] functor below
   doesn't shadow it when later definitions need to reference its module
   types (e.g. [Schema_extended] referring to [Schema.S]). *)
module Schema_intf = Schema

(* Same trick for [Atomic_write]: we define a local functor of that name below,
   so we keep an alias for the outer module's [.S] signature. *)
module Atomic_write_intf = Atomic_write

(** Bridge an Irmin 4 (Eio-typed) merge function into our Lwt-typed {!Merge.t}.
    Takes the type descriptor explicitly because [Merge.f] only exposes the
    merge function, not its underlying type. *)
let merge_of_eio (type_desc : 'a Type.t) (m : 'a Irmin.Merge.t) : 'a Merge.t =
  let f ~old a b =
    Lwt_eio.run_eio (fun () ->
        let old_eio () = await (old ()) in
        Irmin.Merge.f m ~old:old_eio a b)
  in
  Merge.v type_desc f

(** Reverse bridge: take our Lwt-typed merge function and produce an Irmin 4
    (Eio-typed) one. Used when the user provides a custom [Contents.S] or
    [Metadata.S] whose merge needs to be passed to Irmin's functors. *)
let merge_to_eio (type_desc : 'a Type.t) (m : 'a Merge.t) : 'a Irmin.Merge.t =
  let f ~old a b =
    let old_lwt () = Lwt_eio.run_eio (fun () -> old ()) in
    await (Merge.f m ~old:old_lwt a b)
  in
  Irmin.Merge.v type_desc f

(** Lift a Lwt-typed [Metadata.S] to its Irmin 4 counterpart by bridging the
    [merge] field through {!merge_to_eio}. Used when feeding a user's Schema
    into Irmin.Maker which expects an Irmin.Schema.S. *)
module Metadata (M : Metadata.S) : Irmin.Metadata.S with type t = M.t = struct
  type t = M.t

  let t = M.t
  let default = M.default
  let merge = merge_to_eio M.t M.merge
end

(** Same idea for [Contents.S]. *)
module Contents (C : Contents.S) : Irmin.Contents.S with type t = C.t = struct
  type t = C.t

  let t = C.t
  let merge = merge_to_eio Type.(option C.t) C.merge
end

(** Reverse direction: lift an Irmin 4 (Eio-typed) [Indexable.S] into our
    Lwt-typed counterpart by bridging each I/O operation through
    {!Lwt_eio.run_eio}. The [v] / [batch] operations are not in [S] (they come
    from [Of_config] / the backend's [Repo]), so they are not part of the
    adapter; the user is expected to wrap them at their call site. *)
module Indexable_of_eio (M : Irmin.Indexable.S) :
  Indexable.S
    with type 'a t = 'a M.t
     and type key = M.key
     and type hash = M.hash
     and type value = M.value
     and module Key = M.Key = struct
  type 'a t = 'a M.t
  type key = M.key
  type hash = M.hash
  type value = M.value

  module Key = M.Key

  let mem t k = Lwt_eio.run_eio (fun () -> M.mem t k)
  let find t k = Lwt_eio.run_eio (fun () -> M.find t k)
  let add t v = Lwt_eio.run_eio (fun () -> M.add t v)
  let unsafe_add t h v = Lwt_eio.run_eio (fun () -> M.unsafe_add t h v)
  let index t h = Lwt_eio.run_eio (fun () -> M.index t h)
  let close t = Lwt_eio.run_eio (fun () -> M.close t)
  let batch t f = Lwt_eio.run_eio (fun () -> M.batch t (fun rw -> await (f rw)))
end

(** Eio -> Lwt adapter for [Atomic_write.S]. Used to wrap
    [Inner.Backend.Branch]. *)
module Atomic_write_of_eio (M : Irmin.Atomic_write.S) :
  Atomic_write.S
    with type t = M.t
     and type key = M.key
     and type value = M.value
     and type watch = M.watch = struct
  type t = M.t
  type key = M.key
  type value = M.value
  type watch = M.watch

  let mem t k = Lwt_eio.run_eio (fun () -> M.mem t k)
  let find t k = Lwt_eio.run_eio (fun () -> M.find t k)
  let set t k v = Lwt_eio.run_eio (fun () -> M.set t k v)

  let test_and_set t k ~test ~set =
    Lwt_eio.run_eio (fun () -> M.test_and_set t k ~test ~set)

  let remove t k = Lwt_eio.run_eio (fun () -> M.remove t k)
  let list t = Lwt_eio.run_eio (fun () -> M.list t)

  let watch t ?init f =
    Lwt_eio.run_eio (fun () -> M.watch t ?init (fun k d -> await (f k d)))

  let watch_key t k ?init f =
    Lwt_eio.run_eio (fun () -> M.watch_key t k ?init (fun d -> await (f d)))

  let unwatch t w = Lwt_eio.run_eio (fun () -> M.unwatch t w)
  let close t = Lwt_eio.run_eio (fun () -> M.close t)
  let clear t = Lwt_eio.run_eio (fun () -> M.clear t)
end

(** Lift a Lwt-typed [Schema.S] to [Irmin.Schema.S] by bridging Metadata and
    Contents and re-exporting the rest unchanged (Hash / Branch / Info / Path
    are pure types). *)
module Schema (S : Schema.S) :
  Irmin.Schema.S
    with module Hash = S.Hash
     and module Branch = S.Branch
     and module Info = S.Info
     and module Path = S.Path
     and type Metadata.t = S.Metadata.t
     and type Contents.t = S.Contents.t = struct
  module Hash = S.Hash
  module Branch = S.Branch
  module Info = S.Info
  module Path = S.Path
  module Metadata = Metadata (S.Metadata)
  module Contents = Contents (S.Contents)
end

(** Like {!Schema} but produces an [Irmin.Schema.Extended] by adding the [Node]
    and [Commit] sub-functors that backends like [Irmin_pack_unix.Maker]
    require. *)
module Schema_extended (S : Schema_intf.S) :
  Irmin.Schema.Extended
    with module Hash = S.Hash
     and module Branch = S.Branch
     and module Info = S.Info
     and module Path = S.Path
     and type Metadata.t = S.Metadata.t
     and type Contents.t = S.Contents.t = struct
  include Schema (S)

  module Node
      (Contents_key : Irmin.Key.S with type hash = Hash.t)
      (Node_key : Irmin.Key.S with type hash = Hash.t) =
    Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata) (Contents_key)
      (Node_key)

  module Commit
      (Node_key : Irmin.Key.S with type hash = Hash.t)
      (Commit_key : Irmin.Key.S with type hash = Hash.t) =
  struct
    module Inner_maker = Irmin.Commit.Generic_key.Maker (Info)
    include Inner_maker.Make (Hash) (Node_key) (Commit_key)
  end
end

module Content_addressable
    (M : Content_addressable.Maker)
    (H : Irmin.Hash.S)
    (V : Irmin.Type.S) =
struct
  module Lwt_M = M (H) (V)

  type 'a t = 'a Lwt_M.t
  type key = Lwt_M.key
  type value = Lwt_M.value

  let v c = await (Lwt_M.v c)
  let mem t k = await (Lwt_M.mem t k)
  let find t k = await (Lwt_M.find t k)
  let add t v = await (Lwt_M.add t v)
  let unsafe_add t h v = await (Lwt_M.unsafe_add t h v)
  let close t = await (Lwt_M.close t)

  let batch t f =
    await (Lwt_M.batch t (fun rw -> Lwt_eio.run_eio (fun () -> f rw)))
end

module Append_only (M : Append_only.Maker) (K : Irmin.Type.S) (V : Irmin.Type.S) =
struct
  module Lwt_M = M (K) (V)

  type 'a t = 'a Lwt_M.t
  type key = K.t
  type value = V.t

  let v c = await (Lwt_M.v c)
  let mem t k = await (Lwt_M.mem t k)
  let find t k = await (Lwt_M.find t k)
  let add t k v = await (Lwt_M.add t k v)
  let close t = await (Lwt_M.close t)

  let batch t f =
    await (Lwt_M.batch t (fun rw -> Lwt_eio.run_eio (fun () -> f rw)))
end

module Atomic_write
    (M : Atomic_write.Maker)
    (K : Irmin.Type.S)
    (V : Irmin.Type.S) =
struct
  module Lwt_M = M (K) (V)

  type t = Lwt_M.t
  type key = Lwt_M.key
  type value = Lwt_M.value
  type watch = Lwt_M.watch

  let v c = await (Lwt_M.v c)
  let mem t k = await (Lwt_M.mem t k)
  let find t k = await (Lwt_M.find t k)
  let set t k v = await (Lwt_M.set t k v)
  let test_and_set t k ~test ~set = await (Lwt_M.test_and_set t k ~test ~set)
  let remove t k = await (Lwt_M.remove t k)
  let list t = await (Lwt_M.list t)

  let watch t ?init f =
    await (Lwt_M.watch t ?init (fun k d -> Lwt_eio.run_eio (fun () -> f k d)))

  let watch_key t k ?init f =
    await (Lwt_M.watch_key t k ?init (fun d -> Lwt_eio.run_eio (fun () -> f d)))

  let unwatch t w = await (Lwt_M.unwatch t w)
  let close t = await (Lwt_M.close t)
  let clear t = await (Lwt_M.clear t)
end

(** Lift a Lwt-typed [Indexable.S] to its Irmin 4 (Eio-typed) counterpart.
    Mirror of {!Indexable_of_eio}, going the opposite direction: each I/O
    operation is awaited via {!Lwt_eio.Promise.await_lwt}.

    Used when feeding a user-supplied Lwt [Backend.S] into [Irmin.Of_backend]:
    the user's [Contents] / [Node] / [Commit] sub-stores are Lwt-typed
    [Indexable.S] and need to look like [Irmin.Indexable.S] to Irmin 4. *)
module Indexable_to_eio (M : Indexable.S) :
  Irmin.Indexable.S
    with type 'a t = 'a M.t
     and type key = M.key
     and type hash = M.hash
     and type value = M.value
     and module Key = M.Key = struct
  type 'a t = 'a M.t
  type key = M.key
  type hash = M.hash
  type value = M.value

  module Key = M.Key

  let mem t k = await (M.mem t k)
  let find t k = await (M.find t k)
  let add t v = await (M.add t v)
  let unsafe_add t h v = await (M.unsafe_add t h v)
  let index t h = await (M.index t h)
  let close t = await (M.close t)
  let batch t f = await (M.batch t (fun rw -> Lwt_eio.run_eio (fun () -> f rw)))
end

(** Lift a Lwt-typed [Atomic_write.S] to its Irmin 4 (Eio-typed) counterpart.
    Mirror of {!Atomic_write_of_eio}. Used to bridge a user's [Branch] sub-store
    when feeding a Lwt [Backend.S] into [Irmin.Of_backend]. *)
module Atomic_write_to_eio (M : Atomic_write_intf.S) :
  Irmin.Atomic_write.S
    with type t = M.t
     and type key = M.key
     and type value = M.value
     and type watch = M.watch = struct
  type t = M.t
  type key = M.key
  type value = M.value
  type watch = M.watch

  let mem t k = await (M.mem t k)
  let find t k = await (M.find t k)
  let set t k v = await (M.set t k v)
  let test_and_set t k ~test ~set = await (M.test_and_set t k ~test ~set)
  let remove t k = await (M.remove t k)
  let list t = await (M.list t)

  let watch t ?init f =
    await (M.watch t ?init (fun k d -> Lwt_eio.run_eio (fun () -> f k d)))

  let watch_key t k ?init f =
    await (M.watch_key t k ?init (fun d -> Lwt_eio.run_eio (fun () -> f d)))

  let unwatch t w = await (M.unwatch t w)
  let close t = await (M.close t)
  let clear t = await (M.clear t)
end
