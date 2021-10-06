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

open Astring

module type S = Atomic_write.Key

type t =
  [ `Branch of string | `Remote of string | `Tag of string | `Other of string ]
[@@deriving irmin]

let pp_ref ppf = function
  | `Branch b -> Fmt.pf ppf "refs/heads/%s" b
  | `Remote r -> Fmt.pf ppf "refs/remotes/%s" r
  | `Tag t -> Fmt.pf ppf "refs/tags/%s" t
  | `Other o -> Fmt.pf ppf "refs/%s" o

let path l = String.concat ~sep:"/" l

let of_ref str =
  match String.cuts ~sep:"/" str with
  | "refs" :: "heads" :: b -> Ok (`Branch (path b))
  | "refs" :: "remotes" :: r -> Ok (`Remote (path r))
  | "refs" :: "tags" :: t -> Ok (`Tag (path t))
  | "refs" :: o -> Ok (`Other (path o))
  | _ -> Error (`Msg (Fmt.str "%s is not a valid reference" str))

let t = Irmin.Type.like t ~pp:pp_ref ~of_string:of_ref
let master = `Branch Irmin.Branch.String.master

let is_valid = function
  | `Branch s | `Tag s | `Remote s | `Other s -> Irmin.Branch.String.is_valid s
