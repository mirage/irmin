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

open Import

module type Batch = sig
  type 'a t

  val batch : read t -> ([ read | write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The exact
      guarantees depend on the implementation. *)
end

module type Closeable = sig
  type 'a t

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated with [t]. Any operations
      run on a closed handle will raise {!Closed}. *)
end

module type Double_closeable = sig
  type 'a t
  (** The type for stores that can be closed multiple times. *)

  include Closeable with type 'a t := 'a t
  (** @inline *)

  val is_closed : 'a t -> bool
  (** [is_closed t] is true if [close t] has been called at least once. *)

  val check_not_closed : 'a t -> unit
  (** [check_not_closed t] raises [Irmin.Closed] if the store has been closed at
      least once. *)

  (** {2 Low-level Acces} *)

  type 'a raw
  (** The type for the underlying raw store. *)

  val v : ?closed:bool ref -> 'a raw -> 'a t
  (** [v ?closed conf] is a store which allows users to call close multiple
      times without throwing an error. All other operations are checked so that
      they raise [Irmin.Closed] if the store has been closed at least once.

      [closed] is a reference so it can be shared between multiple stores. *)

  val raw : 'a t -> 'a raw * bool ref
  (** [to_raw t] is [t]'s underlying raw store and closing status. *)
end

module type Of_config = sig
  type 'a t

  val v : Conf.t -> read t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)
end

module type Clearable = sig
  type 'a t

  val clear : 'a t -> unit Lwt.t
  (** Clear the store. This operation is expected to be slow. *)
end

module type Sigs = sig
  exception Closed

  module type Batch = sig
    include Batch
    (** @inline *)
  end

  module type Closeable = sig
    include Closeable
    (** @inline *)
  end

  module type Double_closeable = sig
    include Double_closeable
    (** @inline *)
  end

  module type Of_config = sig
    include Of_config
    (** @inline *)
  end

  module type Clearable = sig
    include Clearable
    (** @inline *)
  end
end
