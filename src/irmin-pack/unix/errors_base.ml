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

(* TODO: Remove [`Tmp] everywhere *)

type error =
  [ `Double_close
  | `File_exists of string
  | `Invalid_parent_directory
  | `No_such_file_or_directory
  | `Not_a_file
  | `Read_on_closed
  | `Read_out_of_bounds
  | `Ro_not_allowed
  | `Tmp
  | `Write_on_closed
  | `Invalid_argument
  | `Decoding_error
  | `Not_a_directory of string
  | `Invalid_layout
  | `Corrupted_legacy_file
  | `Pending_flush
  | `Rw_not_allowed
  | `Migration_needed ]
[@@deriving irmin ~pp]
(** [error] is the type of all errors that can occur in a [result], except
    [`Io_misc] which depends on the Io module used. *)

type error' =
  [ `Double_close
  | `File_exists of string
  | `Invalid_parent_directory
  | `No_such_file_or_directory
  | `Not_a_file
  | `Read_on_closed
  | `Read_out_of_bounds
  | `Tmp
  | `Write_on_closed
  | `Invalid_argument
  | `Decoding_error
  | `Not_a_directory of string
  | `Invalid_layout
  | `Corrupted_legacy_file
  | `Pending_flush
  | `Rw_not_allowed
  | `Migration_needed ]
(** [error'] is the payload of the [Io_error] exception.

    [error'] is [error] without [`Ro_not_allowed], because there exist a
    dedicated [RO_not_allowed] exception.

    We can't use polyval inclusion because repr doesn't support it *)

exception Io_error of error'

let () =
  Printexc.register_printer (function
    | Io_error e -> Some (Fmt.str "Io_error: %a" pp_error (e : error' :> error))
    | _ -> None)

exception RO_not_allowed = Irmin_pack.RO_not_allowed
