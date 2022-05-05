(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type S = sig
  include Irmin.Info.S

  val vf : ?author:string -> ('b, Format.formatter, unit, f) format4 -> 'b
end

module Make (I : Irmin.Info.S) = struct
  include I

  let pp_date ppf t =
    let time = t |> date |> Int64.to_float |> Unix.gmtime in
    Fmt.pf ppf "%d-%02d-%02dT%02d:%02d:%02d.00Z" (time.tm_year + 1900)
      (time.tm_mon + 1) time.tm_mday time.tm_hour time.tm_min time.tm_sec

  let pp =
    let open Fmt in
    record
      [
        field "Date" id pp_date;
        field "Author" id pp_author;
        field "Message" id pp_message;
      ]

  let vf ?author fmt =
    Fmt.kstr
      (fun message () ->
        let date = Int64.of_float (Unix.gettimeofday ()) in
        let author =
          match author with
          | Some a -> a
          | None ->
              (* XXX: get "git config user.name" *)
              Printf.sprintf "Irmin %s.[%d]" (Unix.gethostname ())
                (Unix.getpid ())
        in
        v ~author ~message date)
      fmt
end

module Default = Make (Irmin.Info.Default)
