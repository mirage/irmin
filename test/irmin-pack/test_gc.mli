(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

module Gc : sig
  val tests : unit Alcotest.test_case list
end

module Gc_archival : sig
  val tests : unit Alcotest.test_case list
end

module Concurrent_gc : sig
  val tests : unit Alcotest.test_case list
end

module Split : sig
  val tests : unit Alcotest.test_case list
end

module Snapshot : sig
  val tests : unit Alcotest.test_case list
end

module Store : sig
  module S : Irmin_pack.S

  type t

  val config : string -> Irmin.config
  val init_with_config : sw:Eio.Switch.t -> Irmin.config -> t
  val close : t -> unit
  val start_gc : ?unlink:bool -> t -> S.commit -> unit
  val finalise_gc : t -> unit
  val commit_1 : t -> t * S.commit
  val commit_2 : t -> t * S.commit
  val commit_3 : t -> t * S.commit
  val checkout_exn : t -> S.commit -> t
end
