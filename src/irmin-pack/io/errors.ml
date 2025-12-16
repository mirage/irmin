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

(** Finaliser for a function that returns a result and doesn't raise exceptions.

    If the finaliser fails, it is recommended to log the error. *)
let finalise finaliser f =
  let res = f () in
  finaliser res;
  res

(** Finaliser for a function that might raise exceptions. *)
let finalise_exn finaliser f =
  try
    let res = f () in
    finaliser (Some res);
    res
  with exn ->
    finaliser None;
    raise exn

type base_error =
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
  | `Split_forbidden_during_batch
  | `Split_disallowed
  | `Multiple_empty_chunks
  | `Forbidden_during_gc
  | `Multiple_empty_volumes
  | `Volume_missing of string
  | `Add_volume_forbidden_during_gc
  | `Add_volume_requires_lower
  | `Volume_history_newer_than_archived_data of int63 * int63
  | `Lower_has_no_volume
  | `Volume_not_found of string
  | `No_tmp_path_provided ]
[@@deriving irmin ~pp]
(** [base_error] is the type of most errors that can occur in a [result], except
    for errors that have associated exceptions (see below) and backend-specific
    errors (see {!Io_errors}). *)

type closed_error = [ `Closed ] [@@deriving irmin ~pp]
type read_only_error = [ `Ro_not_allowed ] [@@deriving irmin ~pp]
type error = [ base_error | closed_error | read_only_error ]

exception Pack_error of base_error
exception Closed = Irmin.Closed
exception RO_not_allowed = Irmin_pack.RO_not_allowed

(** Error manager *)
module type S = sig
  type t = error

  val pp : Format.formatter -> [< t ] -> unit
  val raise_error : [< t ] -> 'a
  val log_error : string -> [< t ] -> unit
  val catch : (unit -> 'a) -> ('a, [> t ]) result
  val raise_if_error : ('a, [< t ]) result -> 'a
  val log_if_error : string -> (unit, [< t ]) result -> unit
  val to_json_string : (int63, [< t ]) result -> string
  val of_json_string : string -> (int63, [> t ]) result
end

module Base : S with type t = error = struct
  type t = error

  let pp ppf = function
    | #read_only_error as e -> pp_read_only_error ppf e
    | #closed_error as e -> pp_closed_error ppf e
    | #base_error as e -> pp_base_error ppf e

  let raise_error = function
    | #read_only_error -> raise RO_not_allowed
    | #closed_error -> raise Closed
    | #base_error as e -> raise (Pack_error e)

  let log_error context e = [%log.err "%s failed: %a" context pp e]

  let catch f =
    try Ok (f ()) with
    | Pack_error e -> Error (e : base_error :> [> t ])
    | RO_not_allowed -> Error `Ro_not_allowed
    | Closed -> Error `Closed

  let raise_if_error = function Ok x -> x | Error e -> raise_error e

  let log_if_error context = function
    | Ok _ -> ()
    | Error e -> log_error context e

  type err = Pack_error of base_error | Ro_not_allowed | Closed
  [@@deriving irmin]

  let t_to_err = function
    | #read_only_error -> Ro_not_allowed
    | #closed_error -> Closed
    | #base_error as e -> Pack_error e

  let err_to_t = function
    | Closed -> `Closed
    | Ro_not_allowed -> `Ro_not_allowed
    | Pack_error e -> (e : base_error :> [> t ])

  let err_result = Irmin.Type.(result int63 err_t)

  let to_json_string result =
    let convert = Result.map_error t_to_err in
    convert result |> Irmin.Type.to_json_string err_result

  let of_json_string string =
    match (Irmin.Type.of_json_string err_result) string with
    | Error (`Msg _) -> Error `Decoding_error
    | Ok result -> Result.map_error err_to_t result
end

let () =
  Printexc.register_printer (function
    | Pack_error e -> Some (Fmt.str "Pack_error: %a" pp_base_error e)
    | RO_not_allowed -> Some "RO_not_allowed"
    | Closed -> Some "Closed"
    | _ -> None)
