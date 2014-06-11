(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module IrminFS: sig
  (** Create custom filesystem. *)
  module Make (C: IrminFS.Config ): Irmin.BACKEND
  module Make'(C: IrminFS.Config'): Irmin.BACKEND
end

module IrminGit: sig

  (** Same as [IrminGit] but extended with local Git filesystem
      constructors. *)

  module Memory: Irmin.BACKEND
  (** In-memory database, with synchronisation primitives using
      [Lwt_unix]. *)

  module Memory' (C: sig val root: string end): Irmin.BACKEND
  (** Create a in-memory store with a given root path -- stores with
      different roots will not share their contents. *)

  module type Config = sig

    val root: string option
    (** Database root. *)

    val bare: bool
    (** Should we extend the filesystem *)
  end

  module FS (C: Config): Irmin.BACKEND
  (** Local store, using $(pwd) as database root. *)

end

module IrminHTTP: sig

  (** Create an HTTP server. *)

  module Make (S: Irmin.S): IrminHTTP.S with type t = S.t

end

val install_dir_polling_listener: float -> unit
(** Install the directory listener using active polling. The parameter
    is the thread sleep time. *)
