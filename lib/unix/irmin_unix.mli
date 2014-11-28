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

module Irmin_git: sig

  (** Same as [IrminGit] but extended with local Git filesystem
      constructors. *)

  module type S = Irmin_git.S
  (** Same as {{!Irmin_git.S}Irmin_git.S}. *)

  module Memory (C: Irmin.Contents.S): S with type value = C.t
  (** In-memory Git database, with synchronisation primitives using
      [Lwt_unix]. *)

  module Make (C: Irmin.Contents.S): S with type value = C.t
  (** On-disk Git repository, with synchronisation primitives and file
      access using [Lwt_unix] . *)

end

(*
module IrminHTTP: sig

  (** Create an HTTP server. *)

  module Make (S: Irmin.S): IrminHTTP.S with type t = S.t

end
*)

val install_dir_polling_listener: float -> unit
(** Install the directory listener using active polling. The parameter
    is the thread sleep time. *)
