(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module Schema = Irmin.Schema.KV (Irmin.Contents.String)

module Maker (V : Irmin_pack.Version.S) = struct
  module Maker = Irmin_pack.Maker (V) (Irmin_tezos.Conf)
  include Maker.Make (Schema)
end

module Store = Irmin_pack.Checks.Make (Maker)

module TzMaker (V : Irmin_pack.Version.S) = struct
  module Maker = Irmin_pack.V1 (Irmin_tezos.Conf)
  include Maker.Make (Schema)
end

module TzStore = Irmin_pack.Checks.Make (TzMaker)

module Store_layered = struct
  open Irmin_pack_layered.Maker (Irmin_tezos.Conf)
  module S = Make (Schema)
  include Irmin_pack_layered.Checks.Make (Maker) (S)
end

let () =
  match Sys.getenv_opt "PACK" with
  | Some "layered" -> ( match Store_layered.cli () with _ -> .)
  | Some "tezos" -> ( match TzStore.cli () with _ -> .)
  | _ -> ( match Store.cli () with _ -> .)
