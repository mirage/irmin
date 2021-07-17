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

open! Import
include Irmin_layers_intf

module Layer_id = struct
  type t = layer_id [@@deriving irmin]

  let to_string = function
    | `Upper0 -> "upper0"
    | `Upper1 -> "upper1"
    | `Lower -> "lower"

  let pp = Fmt.of_to_string to_string
end

module Maker
    (CA : Irmin.Content_addressable.Maker)
    (AW : Irmin.Atomic_write.Maker) =
struct
  module Maker = Irmin.Maker (CA) (AW)

  type endpoint = Maker.endpoint

  module Make (Schema : Irmin.Schema.S) = struct
    include Maker.Make (Schema)

    let freeze ?min_lower:_ ?max_lower:_ ?min_upper:_ ?max_upper:_ ?recovery:_
        _repo =
      Lwt.fail_with "not implemented"

    type kinded_key =
      | Commit_t of commit_key
      | Node_t of node_key
      | Content_t of contents_key

    let layer_id _repo _store_handle = Lwt.fail_with "not implemented"
    let async_freeze _ = failwith "not implemented"
    let upper_in_use _repo = failwith "not implemented"
    let self_contained ?min:_ ~max:_ _repo = failwith "not implemented"
    let check_self_contained ?heads:_ _ = failwith "not implemented"
    let needs_recovery _ = failwith "not implemented"

    module Private_layer = struct
      module Hook = struct
        type 'a t = unit

        let v _ = failwith "not implemented"
      end

      let wait_for_freeze _ = Lwt.fail_with "not implemented"

      let freeze' ?min_lower:_ ?max_lower:_ ?min_upper:_ ?max_upper:_
          ?recovery:_ ?hook:_ _repo =
        Lwt.fail_with "not implemented"

      let upper_in_use = upper_in_use
    end
  end
end

module Stats = Stats

(* Enforce that {!S} is a sub-type of {!Irmin.Maker}. *)
module Maker_is_a_maker : Irmin.Maker =
  Maker (Irmin_mem.Content_addressable) (Irmin_mem.Atomic_write)
