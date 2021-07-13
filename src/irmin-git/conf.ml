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

module Conf = struct
  include Irmin.Private.Conf.Make ()

  let root = root ()

  let reference : Git.Reference.t Irmin.Type.t =
    let of_string str = Git.Reference.of_string str |> Result.get_ok in
    let to_string r = Git.Reference.to_string r in
    Irmin.Type.(map string) of_string to_string

  let head =
    key ~doc:"The main branch of the Git repository." "head"
      Irmin.Type.(option reference)
      None

  let bare =
    key ~doc:"Do not expand the filesystem on the disk." "bare" Irmin.Type.bool
      false

  let level =
    key ~doc:"The Zlib compression level." "level" Irmin.Type.(option int) None

  let buffers =
    key ~doc:"The number of 4K pre-allocated buffers." "buffers"
      Irmin.Type.(option int)
      None

  let dot_git =
    key
      ~doc:"The location of the .git directory. By default set to [$root/.git]."
      "dot-git"
      Irmin.Type.(option string)
      None
end

include Conf

let v ?head ?bare ?level ?dot_git ?buffers root =
  let module C = Conf in
  let config = C.empty in
  let config = C.add config Conf.root root in
  let config =
    match bare with
    | None -> C.add config Conf.bare (C.default Conf.bare)
    | Some b -> C.add config Conf.bare b
  in
  let config = C.add config Conf.head head in
  let config = C.add config Conf.level level in
  let config = C.add config Conf.dot_git dot_git in
  let config = C.add config Conf.buffers buffers in
  config
