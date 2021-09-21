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

open Irmin.Backend.Conf

let spec = Spec.v "git"

module Key = struct
  let root = root spec

  let reference : Git.Reference.t Irmin.Type.t =
    let of_string str = Git.Reference.of_string str |> Result.get_ok in
    let to_string r = Git.Reference.to_string r in
    Irmin.Type.(map string) of_string to_string

  let head =
    key ~spec ~doc:"The main branch of the Git repository." "head"
      Irmin.Type.(option reference)
      None

  let bare =
    key ~spec ~doc:"Do not expand the filesystem on the disk." "bare"
      Irmin.Type.bool false

  let level =
    key ~spec ~doc:"The Zlib compression level." "level"
      Irmin.Type.(option int)
      None

  let buffers =
    key ~spec ~doc:"The number of 4K pre-allocated buffers." "buffers"
      Irmin.Type.(option int)
      None

  let dot_git =
    key ~spec
      ~doc:"The location of the .git directory. By default set to [$root/.git]."
      "dot-git"
      Irmin.Type.(option string)
      None
end

let init ?head ?bare ?level ?dot_git ?buffers root =
  let module C = Irmin.Backend.Conf in
  let config = C.empty spec in
  let config = C.add config Key.root root in
  let config =
    match bare with
    | None -> C.add config Key.bare (C.default Key.bare)
    | Some b -> C.add config Key.bare b
  in
  let config = C.add config Key.head head in
  let config = C.add config Key.level level in
  let config = C.add config Key.dot_git dot_git in
  let config = C.add config Key.buffers buffers in
  C.verify config
