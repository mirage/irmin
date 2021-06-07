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

module Make (G : Git.S) (C : Irmin.Contents.S) = struct
  module Raw = Git.Value.Make (G.Hash)
  module Key = Irmin.Hash.Make (G.Hash)

  module V = struct
    type t = C.t

    let type_eq = function `Blob -> true | _ -> false

    let of_git = function
      | Git.Value.Blob b -> (
          let str = G.Value.Blob.to_string b in
          match Irmin.Type.of_string C.t str with
          | Ok x -> Some x
          | Error (`Msg e) -> Fmt.invalid_arg "error %s" e)
      | _ -> None

    let to_git b =
      let str = Irmin.Type.to_string C.t b in
      G.Value.blob (G.Value.Blob.of_string str)
  end

  include Content_addressable.Check_closed (Content_addressable.Make (G) (V))

  module Val = struct
    include C

    let to_bin t = Raw.to_raw (V.to_git t)
    let encode_bin = Irmin.Type.stage (fun (t : t) k -> k (to_bin t))

    let decode_bin =
      Irmin.Type.stage @@ fun buf off ->
      Log.debug (fun l -> l "Content.decode_bin");
      match Raw.of_raw_with_header ~off buf with
      | Ok g -> (
          match V.of_git g with
          | Some g -> (String.length buf, g)
          | None -> failwith "wrong object kind")
      | Error (`Msg _) -> failwith "wrong object"

    let size_of = Irmin.Type.Size.custom_dynamic ()
    let t = Irmin.Type.like ~bin:(encode_bin, decode_bin, size_of) t
  end
end
