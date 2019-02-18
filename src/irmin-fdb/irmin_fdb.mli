(** FoundationDB store. *)

val config: unit -> Irmin.config
(** Configuration values. *)

module type IO = Fdb.IO with type 'a t = 'a Lwt.t

module Append_only (IO : IO): Irmin.APPEND_ONLY_STORE_MAKER
(** A FoundationDB store for append-only values. *)

module Atomic_write (IO : IO): Irmin.ATOMIC_WRITE_STORE_MAKER
(** A FoundationDB store with atomic-write guarantees. *)

module Make (IO : IO): Irmin.S_MAKER
(** A FoundationDB Irmin store. *)

module KV (IO : IO): Irmin.KV_MAKER
(** A FoundationDB KV store. *)
