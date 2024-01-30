(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

open Irmin_cli

(* Adding a new content type *)

module Int = struct
  type t = int

  let t = Irmin.Type.int
  let merge = Irmin.Merge.(option (idempotent t))
end

let () =
  Eio_main.run @@ fun env ->
  Irmin_pack_unix.Io.set_env (Eio.Stdenv.fs env);
  Resolver.Contents.add ~default:true "int" (module Int)

module Schema = struct
  module Contents = Int
  module Hash = Irmin.Hash.BLAKE2B
  module Branch = Irmin.Branch.String
  module Path = Irmin.Path.String_list
  module Info = Irmin.Info.Default
  module Metadata = Irmin.Metadata.None
end

(* Adding a new store type *)

module Store = Irmin_mem.Make (Schema)

let store = Resolver.Store.v Irmin_mem.Conf.spec (module Store)

let () =
  Eio_main.run @@ fun env ->
  Irmin_pack_unix.Io.set_env (Eio.Stdenv.fs env);
  Resolver.Store.add ~default:true "mem-int" (Fixed store)
