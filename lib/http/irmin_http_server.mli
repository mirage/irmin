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

(** HTTP server *)

type hooks = {
  update: unit -> unit Lwt.t;
}

module type S = sig

  type t
  (** Database type. *)

  val listen: t -> ?timeout:int -> ?hooks:hooks -> Uri.t -> unit Lwt.t
  (** [start_server t uri] start a server serving the contents of [t]
      at the address [uri]. *)

end

(** {2 Constructor} *)

module type SERVER = sig

  include Cohttp_lwt.Server

  val listen: t -> ?timeout:int -> Uri.t -> unit Lwt.t
  (** Start the server, listening at the given adress. *)

end

module type DATE = sig
  val pretty: int64 -> string
  (** Pretty print a raw date format. *)
end

module Make (HTTP: SERVER) (D: DATE) (S: Irmin.S): S with type t = S.t
(** Create an HTTP server, serving the contents of an Irmin database. *)
