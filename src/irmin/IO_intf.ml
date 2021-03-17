(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type H = sig
  type (+'a, 'io) t
  (** The higher-kinder type of IO promises carrying a value of type ['a]. *)

  (** {2 Computation} *)

  val return : 'a -> ('a, _) t
  val bind : ('a, 'io) t -> ('a -> ('b, 'io) t) -> ('b, 'io) t
  val map : ('a -> 'b) -> ('a, 'io) t -> ('b, 'io) t
  val both : ('a, 'io) t -> ('b, 'io) t -> ('a * 'b, 'io) t

  (** {2 Exceptions} *)

  val fail : exn -> _ t
  val catch : (unit -> ('a, 'io) t) -> (exn -> ('a, 'io) t) -> ('a, 'io) t

  (** {2 Async threads. *)

  val async : (unit -> (unit, 'io) t) -> (unit, 'io) t
end

module type Mutex = sig
  (** Cooperative locks for mutual exclusion *)

  type +'a io
  (** The type for IO effects. *)

  type t
  (** Type of Lwt mutexes *)

  val create : unit -> t
  (** [create ()] creates a new mutex, which is initially unlocked *)

  val is_empty : t -> bool
  (** [is_empty mutex] returns [true] if they are no thread waiting on the
      mutex, and [false] otherwise *)

  val with_lock : t -> (unit -> 'a io) -> 'a io
  (** [with_lock lock f] is used to lock a mutex within a block scope. The
      function [f ()] is called with the mutex locked, and its result is
      returned from the call to [with_lock]. If an exception is raised from f,
      the mutex is also unlocked before the scope of [with_lock] is exited. *)
end

module type Stream = sig
  type +'a io
  (** The type for IO effects. *)

  type 'a t
  (** The type for data streams. *)

  val create : unit -> 'a t * ('a option -> unit)
  (** [create ()] returns a new stream and a push function.

      To notify the stream's consumer of errors, either use a separate
      communication channel, or use a
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#TYPEresult}
      [result]} stream. There is no way to push an exception into a push-stream. *)

  val iter_s : ('a -> unit io) -> 'a t -> unit io
end

module type S0 = sig
  type +'a t
  (** The type of IO promises carrying a value of type ['a]. *)

  include H with type ('a, _) t := 'a t
end

module type S = sig
  include S0
  (** @inline *)

  module Mutex : Mutex with type 'a io := 'a t
  module Stream : Stream with type 'a io := 'a t
end

module type List = sig
  type (+'a, 'io) t

  val iter_s : ('a -> (unit, 'io) t) -> 'a list -> (unit, 'io) t
  val iter_p : ('a -> (unit, 'io) t) -> 'a list -> (unit, 'io) t
  val map_p : ('a -> ('b, 'io) t) -> 'a list -> ('b list, 'io) t
  val map_s : ('a -> ('b, 'io) t) -> 'a list -> ('b list, 'io) t
  val filter_map_s : ('a -> ('b option, 'io) t) -> 'a list -> ('b list, 'io) t
  val filter_map_p : ('a -> ('b option, 'io) t) -> 'a list -> ('b list, 'io) t

  val fold_left_s :
    ('acc -> 'a -> ('acc, 'io) t) -> 'acc -> 'a list -> ('acc, 'io) t

  val for_all_s : ('a -> (bool, 'io) t) -> 'a list -> (bool, 'io) t
  val for_all_p : ('a -> (bool, 'io) t) -> 'a list -> (bool, 'io) t
end

module type Syntax = sig
  type (+'a, 'io) t

  (** {1 Monadic syntax} *)

  val ( let* ) : ('a, 'io) t -> ('a -> ('b, 'io) t) -> ('b, 'io) t
  (** Syntax for {!bind}. *)

  val ( and* ) : ('a, 'io) t -> ('b, 'io) t -> ('a * 'b, 'io) t
  (** Syntax for {!both}. *)

  (** {1 Applicative syntax} *)

  val ( let+ ) : ('a, 'io) t -> ('a -> 'b) -> ('b, 'io) t
  (** Syntax for {!map}. *)

  val ( and+ ) : ('a, 'io) t -> ('b, 'io) t -> ('a * 'b, 'io) t
  (** Syntax for {!both}. *)
end

type (+'a, _) t

module type Higher = sig
  module Higher : sig
    type s

    include S with type 'a t = ('a, s) t
    (** @inline *)

    module Syntax : Syntax with type ('a, _) t := 'a t
  end

  include S
  (** @inline *)

  module Syntax : Syntax with type ('a, _) t := 'a t

  (** {2 Conversion between kinds. *)

  external inj : 'a t -> 'a Higher.t = "%identity"
  external prj : 'a Higher.t -> 'a t = "%identity"
end
