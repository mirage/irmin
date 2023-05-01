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

open Import
open Errors

(** Error manager for errors and exceptions defined in {!Errors} and
    {!Io.S.misc_error} *)
module type S = sig
  module Io : Io.S

  type t = [ Base.t | `Io_misc of Io.misc_error ] [@@deriving irmin]

  val raise_error : [< t ] -> 'a
  val log_error : string -> [< t ] -> unit
  val catch : (unit -> 'a) -> ('a, t) result
  val raise_if_error : ('a, [< t ]) result -> 'a
  val log_if_error : string -> ('a, [< t ]) result -> unit
end

module Make (Io : Io.S) : S with module Io = Io = struct
  module Io = Io

  (* Inline the definition of the polymorphic variant for the ppx. *)
  type t =
    [ `Double_close
    | `File_exists of string
    | `Invalid_parent_directory
    | `No_such_file_or_directory of string
    | `Not_a_file
    | `Read_out_of_bounds
    | `Invalid_argument
    | `Decoding_error
    | `Not_a_directory of string
    | `Index_failure of string
    | `Invalid_layout
    | `Corrupted_legacy_file
    | `Corrupted_mapping_file of string
    | `Pending_flush
    | `Rw_not_allowed
    | `Migration_needed
    | `Migration_to_lower_not_allowed
    | `Corrupted_control_file of string
    | `Sys_error of string
    | `V3_store_from_the_future
    | `Gc_forbidden_during_batch
    | `Unknown_major_pack_version of string
    | `Only_minimal_indexing_strategy_allowed
    | `Commit_key_is_dangling of string
    | `Dangling_key of string
    | `Gc_disallowed of string
    | `Node_or_contents_key_is_indexed of string
    | `Gc_process_error of string
    | `Corrupted_gc_result_file of string
    | `Gc_process_died_without_result_file of string
    | `Gc_forbidden_on_32bit_platforms
    | `Invalid_prefix_read of string
    | `Invalid_sparse_read of [ `After | `Before | `Hole ] * int63
    | `Invalid_volume_read of [ `Empty | `Closed ] * int63
    | `Inconsistent_store
    | `Closed
    | `Ro_not_allowed
    | `Io_misc of Io.misc_error
    | `Split_forbidden_during_batch
    | `Split_disallowed
    | `Multiple_empty_chunks
    | `Forbidden_during_gc
    | `Multiple_empty_volumes
    | `Volume_missing of string
    | `Volume_history_newer_than_archived_data of int63 * int63
    | `Lower_has_no_volume
    | `Add_volume_forbidden_during_gc
    | `Add_volume_requires_lower
    | `Volume_not_found of string
    | `No_tmp_path_provided ]
  [@@deriving irmin]

  let raise_error = function
    | `Io_misc e -> Io.raise_misc_error e
    | #error as e -> Base.raise_error e

  let log_error context e =
    [%log.err "%s failed: %a" context (Irmin.Type.pp t) (e :> t)]

  let catch f =
    match Base.catch (fun () -> Io.catch_misc_error f) with
    | Ok (Ok v) -> Ok v
    | Ok (Error e) -> Error (e :> t)
    | Error e -> Error (e :> t)

  let raise_if_error = function Ok x -> x | Error e -> raise_error e

  let log_if_error context = function
    | Ok _ -> ()
    | Error e -> log_error context e
end
