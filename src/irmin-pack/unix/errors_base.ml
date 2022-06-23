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

type base_error =
  [ `File_exists of string
  | `Invalid_parent_directory
  | `No_such_file_or_directory
  | `Not_a_file
  | `Read_on_closed
  | `Read_out_of_bounds
  | `Write_on_closed
  | `Invalid_argument
  | `Decoding_error
  | `Not_a_directory of string
  | `Index_failure of string
  | `Invalid_layout
  | `Corrupted_legacy_file
  | `Pending_flush
  | `Rw_not_allowed
  | `Migration_needed
  | `Corrupted_control_file
  | `Sys_error of string
  | `V3_store_from_the_future
  | `Gc_forbidden_during_batch
  | `Unknown_major_pack_version of string
  | `Only_minimal_indexing_strategy_allowed
  | `Commit_key_is_indexed_and_dangling of string
  | `Dangling_key of string
  | `Gc_disallowed
  | `Node_or_contents_key_is_indexed of string
  | `Commit_parent_key_is_indexed of string ]
[@@deriving irmin ~pp]
(** [base_error] is the type of most errors that can occur in a [result], except
    [`Io_misc] which depends on the Io module used, and except [`Ro_not_allowed]
    which has a dedicated exception. *)

exception Pack_error of base_error

let () =
  Printexc.register_printer (function
    | Pack_error e -> Some (Fmt.str "Pack_error: %a" pp_base_error e)
    | _ -> None)

exception RO_not_allowed = Irmin_pack.RO_not_allowed
