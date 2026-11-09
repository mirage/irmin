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

module type S = sig
  (** {1 Watch Helpers} *)

  type key
  (** The type for store keys. *)

  type value
  (** The type for store values. *)

  type watch
  (** The type for watch handlers. *)

  type t
  (** The type for watch state. *)

  val stats : t -> int * int
  (** [stats t] is a tuple [(k,a)] represeting watch stats. [k] is the number of
      single key watchers for the store [t] and [a] the number of global
      watchers for [t]. *)

  val notify : t -> key -> value option -> unit
  (** Notify all listeners in the given watch state that a key has changed, with
      the new value associated to this key. [None] means the key has been
      removed. *)

  val v : unit -> t
  (** Create a watch state. *)

  val clear : t -> unit
  (** Clear all register listeners in the given watch state. *)

  val watch_key : t -> key -> ?init:value -> (value Diff.t -> unit) -> watch
  (** Watch a given key for changes. More efficient than {!watch}.

      @raise Invalid_argument if [set_watch_switch] was not called. *)

  val watch :
    t -> ?init:(key * value) list -> (key -> value Diff.t -> unit) -> watch
  (** Add a watch handler. To watch a specific key, use {!watch_key} which is
      more efficient.

      @raise Invalid_argument if [set_watch_switch] was not called. *)

  val unwatch : t -> watch -> unit
  (** Remove a watch handler. *)

  val listen_dir :
    t ->
    string ->
    key:(string -> key option) ->
    value:(key -> value option) ->
    unit ->
    unit
  (** Register a thread looking for changes in the given directory and return a
      function to stop watching and free up resources.

      @raise Invalid_argument if [set_watch_switch] was not called. *)
end

module type Sigs = sig
  module type S = S
  (** The signature for watch helpers. *)

  val workers : unit -> int
  (** [workers ()] is the number of background worker threads managing event
      notification currently active. *)

  type hook = int -> string -> (string -> unit) -> unit -> unit
  (** The type for watch hooks. [h id dir f] starts watching the directory
      [dir], calls [f] with the path of every changed file relative to [dir],
      and returns a function to stop watching. *)

  val none : hook
  (** [none] is the hooks which asserts false. *)

  val set_watch_switch : Eio.Switch.t -> unit
  (** [set_watch_switch sw] sets the switch of the fibers that run the watch
      callbacks. It must be set before adding a watch or listening to a
      directory, which raise [Invalid_argument] until then. [sw] must outlive
      the watches: once it is off, the notifications are dropped and logged. A
      notification that would start a worker from another domain than the one of
      [sw] is dropped and logged as an error. A running worker keeps its switch
      until it is off or the watches are removed. The callbacks of a watch state
      run one at a time and in order, so a callback that does not return delays
      the next ones, even on a new switch. {!set_listen_dir_eio_hook} also sets
      it. *)

  val set_listen_dir_hook : hook -> unit
  (** Register a function which looks for file changes in a directory and return
      a function to stop watching. It is probably best to use
      {!set_listen_dir_eio_hook} with [Irmin_watcher.hook] there. By default, it
      uses {!none}. Unlike {!set_listen_dir_eio_hook}, it does not set
      {!set_watch_switch}. *)

  type eio_hook =
    int -> Eio.Fs.dir_ty Eio.Path.t -> (string -> unit) -> unit -> unit
  (** The type for watch hooks on Eio paths. Like {!hook}, the callback receives
      paths relative to the watched directory. *)

  val set_listen_dir_eio_hook :
    sw:Eio.Switch.t -> Eio.Fs.dir_ty Eio.Path.t -> eio_hook -> unit
  (** [set_listen_dir_eio_hook ~sw fs h] is {!set_listen_dir_hook} for the hook
      [h] on Eio paths, such as [Irmin_watcher.hook w], and also sets
      {!set_watch_switch} to [sw]. The directories to watch are resolved
      relative to [fs], usually [Eio.Stdenv.fs env]: a capability restricted to
      a directory, such as [Eio.Stdenv.cwd env], may refuse absolute paths. [sw]
      is usually the switch of the watcher behind [h]. [sw] and that watcher
      must outlive the watches. *)

  (** [Make] builds an implementation of watch helpers. *)
  module Make (K : Type.S) (V : Type.S) :
    S with type key = K.t and type value = V.t
end
