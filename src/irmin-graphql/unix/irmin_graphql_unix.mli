(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type remote_fn =
  ?ctx:Mimic.ctx -> ?headers:Cohttp.Header.t -> string -> unit -> Irmin.remote

module Server : sig
  module Remote : sig
    module None : sig
      val remote : remote_fn option
    end
  end

  module Make
      (S : Irmin.Generic_key.S) (Remote : sig
        val remote : remote_fn option
      end) :
    Irmin_graphql.Server.S
      with type repo = S.repo
       and type server = Cohttp_lwt_unix.Server.t

  module Make_ext
      (S : Irmin.Generic_key.S) (Remote : sig
        val remote : remote_fn option
      end)
      (T : Irmin_graphql.Server.CUSTOM_TYPES
             with type path := S.path
              and type metadata := S.metadata
              and type contents := S.contents
              and type hash := S.hash
              and type branch := S.branch
              and type commit_key := S.commit_key
              and type contents_key := S.contents_key
              and type node_key := S.node_key) :
    Irmin_graphql.Server.S
      with type repo = S.repo
       and type server = Cohttp_lwt_unix.Server.t
end
