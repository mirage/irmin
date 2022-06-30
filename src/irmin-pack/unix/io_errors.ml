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

  type t = [ Base.t | `Io_misc of Io.misc_error ]

  val pp : Format.formatter -> [< t ] -> unit
  val raise_error : [< t ] -> 'a
  val log_error : string -> [< t ] -> unit
  val catch : (unit -> 'a) -> ('a, [> t ]) result
  val raise_if_error : ('a, [< t ]) result -> 'a
  val log_if_error : string -> (unit, [< t ]) result -> unit
  val to_json_string : (int63, [< t ]) result -> string
  val of_json_string : string -> (int63, [> t ]) result
end

module Make (Io : Io.S) : S with module Io = Io = struct
  module Io = Io

  type misc_error = Io.misc_error [@@deriving irmin ~pp]
  type io_error = [ `Io_misc of misc_error ] [@@deriving irmin]
  type t = [ Base.t | io_error ]

  let pp ppf = function
    | `Io_misc e -> pp_misc_error ppf e
    | #error as e -> Base.pp ppf e

  let raise_error = function
    | `Io_misc e -> Io.raise_misc_error e
    | #error as e -> Base.raise_error e

  let log_error context e = [%log.err "%s failed: %a" context pp e]

  let catch f =
    try Io.catch_misc_error f with _ as ex -> Base.catch (fun () -> raise ex)

  let raise_if_error = function Ok x -> x | Error e -> raise_error e

  let log_if_error context = function
    | Ok _ -> ()
    | Error e -> log_error context e

  let io_err_result = Irmin.Type.(result int63 io_error_t)

  let to_json_string result =
    match result with
    | Ok _ as v -> v |> Irmin.Type.to_json_string io_err_result
    | Error e -> (
        match e with
        | `Io_misc _ as e -> Error e |> Irmin.Type.to_json_string io_err_result
        | #error as e -> Error e |> Base.to_json_string)

  let of_json_string string =
    match Irmin.Type.of_json_string io_err_result string with
    | Error (`Msg _) -> Base.of_json_string string
    | Ok result -> (result : (_, io_error) result :> (_, [> t ]) result)
end
