(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
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

(** [Time] specifies a source of timestamps. Timestamps must be monotonic for
    the data structures to function properly. *)

(** Signature for the timestamps *)
module type S = sig
  include Irmin.Type.S
  (** Type of the timestamp *)

  val now : unit -> t
  (** Returns a timestamp *)
end

module Machine : S
(** A timestamp method using system-specific monotonic clocks (as provided by
    the [Mtime] package). *)
