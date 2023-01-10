(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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
include Logging_intf
module Source_code_position = Ppx_irmin_internal_lib.Source_code_position

(** A logs reporter that is aware of the tags added by [ppx_irmin.internal]. *)
let reporter :
    ?filter_src:(Logs.src -> bool) ->
    ?prefix:string ->
    (module Clock) ->
    Logs.reporter =
 fun ?(filter_src = Fun.const true) ?(prefix = "") (module Clock) ->
  let pad n x =
    if String.length x > n then x else x ^ String.make (n - String.length x) ' '
  in
  let start_time = Clock.counter () in
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h tags k fmt =
      let dt = Mtime.span_to_us (Clock.count start_time) in
      let source_pos_text, source_pos_colour =
        match tags with
        | None -> (Logs.Src.name src, `Magenta)
        | Some tags ->
            let text =
              Logs.Tag.find Source_code_position.tag tags
              |> Option.fold ~none:"" ~some:(fun (fname, lnum, _, _) ->
                     Fmt.str "%s:%d" fname lnum)
            in
            (text, `Faint)
      in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt
        Fmt.(styled source_pos_colour string)
        (pad 35 source_pos_text) Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    if filter_src src then with_stamp header tags k fmt
    else Format.ikfprintf k ppf fmt
  in
  { Logs.report }
