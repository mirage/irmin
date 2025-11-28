(*
 * Copyright (c) 2023 Tarides <contact@tarides.com>
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

module Internal = Irmin.Backend.Lru.Make (struct
  include Int63

  let hash = Hashtbl.hash
end)

type key = int63
type value = Irmin_pack.Pack_value.kinded
type t = { weighted : bool; lru : value Internal.t }

let create config =
  let lru_max_memory = Irmin_pack.Conf.lru_max_memory config in
  let weighted, weight_limit =
    match lru_max_memory with
    | None -> (false, Some (Irmin_pack.Conf.lru_size config))
    | Some _ -> (true, lru_max_memory)
  in
  { weighted; lru = Internal.create weight_limit }

(** [exceeds_entry_weight_limit] attempts to filter out entries that are "too
    large".

    Since we do not necessarily want to incur a cost for calculating the weight
    for every entry when [lru_max_memory] is not configured, the control for
    this is in the caller of [add]. Only [Irmin_pack.Pack_value.Immediate]
    weight's are checked.

    The current entry weight limit is hard-coded to 20kB. *)
let exceeds_entry_weight_limit = function
  | Irmin_pack.Pack_value.Immediate w -> w > 20_000
  | Deferred _ -> false

let resolve_weight = function
  | Irmin_pack.Pack_value.Immediate w -> w
  | Deferred w -> w ()

let add t k w v =
  if t.weighted && exceeds_entry_weight_limit w then ()
  else if t.weighted then
    let w = resolve_weight w in
    Internal.add t.lru k ~weight:w v
  else Internal.add t.lru k v

let find lru = Internal.find lru.lru
let mem lru = Internal.mem lru.lru
let clear lru = Internal.clear lru.lru
let iter lru = Internal.iter lru.lru
