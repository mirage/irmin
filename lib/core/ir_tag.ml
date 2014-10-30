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

open Lwt

module Log = Log.Make(struct let section = "TAG" end)

module type S = sig
  include Tc.I0
  val master: t
end

module String = struct
  type t = string

  let hash = Hashtbl.hash
  let equal = (=)
  let compare = Pervasives.compare

  let implode ts = String.concat "/" ts
  let explode t = List.filter ((<>)"") (Stringext.split t ~on:'/')
  let to_json t = Ezjsonm.(list encode_string (explode t))
  let of_json j = implode (Ezjsonm.get_list Ezjsonm.decode_string_exn j)
  let to_sexp t = Sexplib.Conv.(sexp_of_list sexp_of_string (explode t))
  let size_of = String.length
  let master = "master"

  let write t buf =
    let len = String.length t in
    Cstruct.blit_from_string t 0 buf 0 len;
    Cstruct.shift buf len

  let read buf =
    match Mstruct.get_string_delim buf '\000' with
    | None   -> Mstruct.get_string buf (Mstruct.length buf)
    | Some s -> s
end

module type STORE = sig
  include Ir_rw.S
  module Key: S with type t = key
  module Value: Ir_uid.S with type t = value
end

module Make
    (K: S)
    (V: Ir_uid.S)
    (S: Ir_rw.S with type key = K.t and type value = V.t)
= struct
  module Key = K
  module Value = V
  include S

  (* XXX: here, all the tags are public, is it what we want? *)
  let list t _ =
    dump t >>= fun l ->
    return (List.map fst l)
end
