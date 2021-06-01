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

include Xgit_intf

let src = Logs.Src.create "git.unix" ~doc:"logs git's unix events"

module Log = (val Logs.src_log src : Logs.LOG)

let remote ?(ctx = Mimic.empty) ?headers uri =
  let ( ! ) f a b = f b a in
  let headers = Option.map Cohttp.Header.to_list headers in
  match Smart_git.Endpoint.of_string uri with
  | Ok edn ->
      let edn =
        Option.fold ~none:edn
          ~some:(!Smart_git.Endpoint.with_headers_if_http edn)
          headers
      in
      (ctx, edn)
  | Error (`Msg err) -> Fmt.invalid_arg "remote: %s" err

module Maker (G : Irmin_git.G) = struct
  module G = G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  module Maker = struct
    module S = Irmin_git.Maker (G) (Git_unix.Sync (G) (Git_cohttp_unix))
    module KV = Irmin_git.KV (G) (Git_unix.Sync (G) (Git_cohttp_unix))
    module Ref = Irmin_git.Ref (G) (Git_unix.Sync (G) (Git_cohttp_unix))
  end

  module Make
      (S : Irmin_git.Schema.S
             with type hash = G.hash
              and type node = G.Value.Tree.t
              and type commit = G.Value.Commit.t) =
  struct
    include Maker.S.Make (S)

    let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
  end

  module KV (C : Irmin.Contents.S) = struct
    include Maker.KV.Make (C)

    let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
  end

  module Ref (C : Irmin.Contents.S) = struct
    include Maker.Ref.Make (C)

    let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
  end
end

module FS = struct
  include Maker (Git_unix.Store)
  module G = Git_unix.Store
end

module Mem = struct
  include Maker (Irmin_git.Mem)
  module G = Irmin_git.Mem
end
