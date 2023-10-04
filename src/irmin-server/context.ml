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

module Make (IO : Conn.IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) =
struct
  module Server_info = struct
    type t = { start_time : float }

    let uptime { start_time; _ } = IO.time () -. start_time
  end

  module Conn = Conn.Make (IO) (Codec)

  type context = {
    conn : Conn.t;
    config : Irmin.Backend.Conf.t;
    repo : Store.Repo.t;
    mutable watch : Store.watch option;
    mutable branch_watch : Store.Backend.Branch.watch option;
  }

  module type CMD = sig
    type req
    type res

    val req_t : req Irmin.Type.t
    val res_t : res Irmin.Type.t
    val name : string

    val run :
      Conn.t -> context -> Server_info.t -> req -> res Conn.Return.t Lwt.t
  end

  let cmd (module C : CMD) = (C.name, (module C : CMD))
end
