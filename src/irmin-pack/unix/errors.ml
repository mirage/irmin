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
include Errors_base

(** Finaliser for a function that returns a result and doesn't raise exceptions.

    If the finaliser fails, it is recommended to log the error. *)
let finalise finaliser f =
  let res = f () in
  finaliser res;
  res

module type S = sig
  (** Error manager for [RO_not_allowed], [Pack_error], [Io.misc_error] and IO's
      dedicated exceptions *)

  module Io : Io.S

  type t = [ base_error | `Io_misc of Io.misc_error | `Ro_not_allowed ]

  val pp : Format.formatter -> [< t ] -> unit
  val raise_error : [< t ] -> 'a
  val catch : (unit -> 'a) -> ('a, [> t ]) result
  val raise_if_error : ('a, [< t ]) result -> 'a
  val log_if_error : string -> (unit, [< t ]) result -> unit
  val to_json_string : (int63, t) result -> string
  val of_json_string : string -> (int63, t) result
end

module Make (Io : Io.S) : S with module Io = Io = struct
  module Io = Io

  type t = [ base_error | `Io_misc of Io.misc_error | `Ro_not_allowed ]
  type misc_error = Io.misc_error [@@deriving irmin ~pp]

  let pp ppf = function
    | `Ro_not_allowed -> Format.fprintf ppf "`Ro_not_allowed"
    | `Io_misc e -> Format.fprintf ppf "%a" pp_misc_error e
    | #base_error as e -> Format.fprintf ppf "%a" pp_base_error e

  let raise_error = function
    | `Io_misc e -> Io.raise_misc_error e
    | `Ro_not_allowed -> raise RO_not_allowed
    | #base_error as e -> raise (Pack_error e)

  let catch f =
    try Io.catch_misc_error f with
    | Pack_error e -> Error (e : base_error :> [> base_error ])
    | RO_not_allowed -> Error `Ro_not_allowed

  let raise_if_error = function Ok x -> x | Error e -> raise_error e

  let log_if_error context = function
    | Ok () -> ()
    | Error e -> [%log.err "%s failed: %a" context pp e]

  type err =
    | Pack_error of base_error
    | Io_misc of Io.misc_error
    | Ro_not_allowed
  [@@deriving irmin]

  let t_to_err = function
    | `Io_misc e -> Io_misc e
    | `Ro_not_allowed -> Ro_not_allowed
    | #base_error as e -> Pack_error e

  let err_to_t = function
    | Io_misc e -> `Io_misc e
    | Ro_not_allowed -> `Ro_not_allowed
    | Pack_error e -> (e : base_error :> [> base_error ])

  let err_result = Irmin.Type.(result int63 err_t)

  let to_json_string result =
    let convert = Result.map_error t_to_err in
    convert result |> Irmin.Type.to_json_string err_result

  let of_json_string string =
    match (Irmin.Type.of_json_string err_result) string with
    | Error (`Msg _) -> Error `Decoding_error
    | Ok result -> Result.map_error err_to_t result
end
