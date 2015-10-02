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

(** HTTP helpers *)

val truncate: string -> int -> string

val ok_or_error: [`Ok | `Error] Tc.t
val ok_or_duplicated_branch_id: [`Ok | `Duplicated_branch | `Empty_head] Tc.t
val lca: 'a Tc.t -> [`Ok of 'a list | `Max_depth_reached | `Too_many_lcas] Tc.t

val start_stream: string
val stop_stream: string
val irmin_version: string

val content_type_header: string
val application_json: string
val application_octet_stream: string

val json_headers: Cohttp.Header.t
val html_headers: Cohttp.Header.t
val raw_headers: Cohttp.Header.t

type ct = [`Json | `Raw]

val ct_of_header: Cohttp.Header.t -> ct
val header_of_ct: ct -> string

val string_of_ct: ct -> string
val ct_of_string: string -> ct option

type contents

val raw_contents: 'a Tc.t -> 'a -> contents
val json_contents: 'a Tc.t -> 'a -> contents
val json: Ezjsonm.value -> contents

val ct_of_contents: contents option -> ct
val contents: 'a Tc.t -> contents -> 'a

module Request: sig
  type t = Irmin.task * contents option

  val to_body: ct -> t -> Cohttp_lwt_body.t
  val of_body: meth:string -> ct -> Cohttp_lwt_body.t -> t option Lwt.t

end

module Response: sig

  exception Server_error of string
  exception Client_error of string

  type t = [ `Error of exn | `Ok of contents ]

  val to_body: ct -> t -> Cohttp.Header.t * Cohttp_lwt_body.t
  val of_body: ct -> Cohttp_lwt_body.t -> t Lwt.t

  val of_json: version:bool -> Ezjsonm.value -> t
end
