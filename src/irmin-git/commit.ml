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

open Import

module Make (G : Git.S) = struct
  module Info = Irmin.Info.Default
  module Raw = Git.Value.Make (G.Hash)
  module Key = Irmin.Hash.Make (G.Hash)

  module V = struct
    type t = G.Value.Commit.t

    let type_eq = function `Commit -> true | _ -> false
    let of_git = function Git.Value.Commit c -> Some c | _ -> None
    let to_git c = G.Value.commit c
  end

  include Content_addressable.Check_closed (Content_addressable.Make (G) (V))

  module Val = struct
    module Info = Info

    type t = G.Value.Commit.t
    type hash = Key.t [@@deriving irmin]

    let info_of_git author message =
      let id = author.Git.User.name in
      let date, _ = author.Git.User.date in
      (* FIXME: tz offset is ignored *)
      Info.v ~author:id ~message date

    let name_email name =
      let name = String.trim name in
      try
        let i = String.rindex name ' ' in
        let email = String.sub name (i + 1) (String.length name - i - 1) in
        if
          String.length email > 0
          && email.[0] = '<'
          && email.[String.length email - 1] = '>'
        then
          let email = String.sub email 1 (String.length email - 2) in
          let name = String.trim (String.sub name 0 i) in
          (name, email)
        else (name, "irmin@openmirage.org")
      with Not_found -> (name, "irmin@openmirage.org")

    let of_git g =
      let node = G.Value.Commit.tree g in
      let parents = G.Value.Commit.parents g in
      let author = G.Value.Commit.author g in
      let message = G.Value.Commit.message g in
      let message = Option.value ~default:"" message in
      let info = info_of_git author message in
      (info, node, parents)

    let to_git info node parents =
      let tree = node in
      let parents = List.fast_sort G.Hash.compare parents in
      let author =
        let date = Info.date info in
        let name, email = name_email (Info.author info) in
        Git.User.{ name; email; date = (date, None) }
      in
      let message = Info.message info in
      G.Value.Commit.make (* FIXME: should be v *) ~tree ~parents ~author
        ~committer:author
        (if message = "" then None else Some message)

    let v ~info ~node ~parents = to_git info node parents
    let xnode g = G.Value.Commit.tree g
    let node t = xnode t
    let parents g = G.Value.Commit.parents g

    let info g =
      let author = G.Value.Commit.author g in
      let message = Option.value ~default:"" (G.Value.Commit.message g) in
      info_of_git author message

    module C = Irmin.Private.Commit.Make (Key)

    let of_c c = to_git (C.info c) (C.node c) (C.parents c)

    let to_c t =
      let info, node, parents = of_git t in
      C.v ~info ~node ~parents

    let to_bin t = Raw.to_raw (G.Value.commit t)

    let encode_bin =
      Irmin.Type.stage @@ fun (t : t) k ->
      Log.debug (fun l -> l "Commit.encode_bin");
      k (to_bin t)

    let decode_bin =
      Irmin.Type.stage @@ fun buf off ->
      Log.debug (fun l -> l "Commit.decode_bin");
      match Raw.of_raw_with_header ~off buf with
      | Ok (Git.Value.Commit t) -> (String.length buf, t)
      | Ok _ -> failwith "wrong object kind"
      | Error _ -> failwith "wrong object kind"

    let size_of = Irmin.Type.Size.custom_dynamic ()
    let t = Irmin.Type.map ~bin:(encode_bin, decode_bin, size_of) C.t of_c to_c
  end
end
