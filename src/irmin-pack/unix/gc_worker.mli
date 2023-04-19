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

(** Code for running in an async GC worker thread. *)

open! Import
module Payload = Control_file.Payload.Upper.Latest

module Make (Args : Gc_args.S) : sig
  module Args : Gc_args.S

  val run_and_output_result :
    lower_root:string option ->
    generation:int ->
    new_files_path:string ->
    string ->
    Args.key ->
    int63 ->
    unit

  type suffix_params = {
    start_offset : int63;
    chunk_start_idx : int;
    dead_bytes : int63;
  }
  [@@deriving irmin]

  type gc_results = {
    suffix_params : suffix_params;
    mapping_size : int63;
    removable_chunk_idxs : int list;
    modified_volume : Lower.volume_identifier option;
    stats : Stats.Latest_gc.worker;
  }
  [@@deriving irmin]

  type gc_output = (gc_results, Args.Errs.t) result [@@deriving irmin]
end
with module Args := Args
