(*
 * Copyright (c) 2026 Tarides <contact@tarides.com>
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

(** The origins of the web pages, as the browsers send them in the [Origin]
    header (RFC 6454). *)

type t
(** An origin: a scheme, a host and an optional port. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the origin written [s], like [https://example.org] or
    [http://localhost:8080]. The scheme and the host are not case sensitive, the
    default port of [http] and [https] is optional, and a final [/] is allowed.
    The host is written as the browsers send it: an ASCII name (punycode for an
    internationalised name), an IPv4 address in dotted decimal like [127.0.0.1],
    or an IPv6 address in brackets in its canonical form like [[::1]] (no
    leading zero, the longest run of zero groups written [::]). Other forms are
    refused, because they would never match. [null], the origin of the pages
    without a proper origin (like the [file:] pages), is not an origin. *)

val to_string : t -> string
(** [to_string t] is [t] with a scheme and a host in lowercase, and without the
    default port of the scheme. *)

val equal : t -> t -> bool
(** [equal t t'] is [true] when [t] and [t'] are the same origin. *)

val pp : t Fmt.t
(** [pp] formats an origin with {!to_string}. *)
