(*
 * Copyright (c) 2026 Tarides
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

(* Lwt-flavoured shim over [Irmin_client_unix]. Takes a Lwt-typed
   [Contents.S], synthesises an Eio-side reference store (using [Irmin_mem.KV]
   purely as a Schema carrier — no actual local backing happens), feeds it to
   [Irmin_client_unix.Make] to obtain an Eio-typed RPC client store, and
   wraps the result back to [Irmin_lwt.Generic_key.S] via [Wrap_store.Make].
   Client-specific extras (connect / reconnect / dup / Batch / etc.) are
   passed through. *)

let run = Lwt_eio.run_eio

module Error = Irmin_client_unix.Error

let config = Irmin_client_unix.config

module Make_codec (Codec : Irmin_server.Conn.Codec.S) (V : Irmin_lwt.Contents.S) =
struct
  module V_eio = Irmin_lwt.Lwt_to_eio.Contents (V)

  (* Reference Eio Store: only used for its Schema by [Irmin_client_unix].
     The local mem backend never holds real data because the client routes
     all ops over the wire. *)
  module Reference = Irmin_mem.KV.Make (V_eio)
  module Inner = Irmin_client_unix.Make_codec (Codec) (Reference)

  (* Lwt-typed Schema. Pure modules (Hash / Branch / Info / Path) are
     reused directly from the Eio Schema. [Metadata = unit] (Metadata.None),
     so we use the Lwt-side [Metadata.None] which has the same [t = unit]. *)
  module Schema_lwt = struct
    module Hash = Inner.Schema.Hash
    module Branch = Inner.Schema.Branch
    module Info = Inner.Schema.Info
    module Path = Inner.Schema.Path
    module Metadata = Irmin_lwt.Metadata.None
    module Contents = V
  end

  include Irmin_lwt.Wrap_store.Make (Schema_lwt) (Inner.Schema) (Inner)

  (* Pass-through client-specific surface. The functions are partly Eio
     and partly Lwt upstream (Lwt because the network IO uses cohttp-lwt
     / websocket-lwt). We keep the upstream shape, wrapping Eio-direct
     ones in [Lwt.t] for caller convenience and leaving the already-Lwt
     ones unchanged. *)

  let connect ?tls ?hostname uri =
    run (fun () -> Inner.connect ?tls ?hostname uri)

  let reconnect = Inner.reconnect
  let uri = Inner.uri
  let close r = run (fun () -> Inner.close r)
  let dup = Inner.dup
  let ping r = run (fun () -> Inner.ping r)
  let export ?depth r = run (fun () -> Inner.export ?depth r)
  let import r s = run (fun () -> Inner.import r s)

  module Batch = struct
    include Inner.Batch

    let apply ~info ?path s b =
      run (fun () -> Inner.Batch.apply ~info ?path s b)
  end
end

module Make (V : Irmin_lwt.Contents.S) =
  Make_codec (Irmin_server.Conn.Codec.Bin) (V)

module Make_json (V : Irmin_lwt.Contents.S) =
  Make_codec (Irmin_server.Conn.Codec.Json) (V)
