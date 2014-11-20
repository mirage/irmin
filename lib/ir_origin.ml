(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Printf

module type S = sig
  include Tc.I0
  val create: ?date:int64 -> ?id:string -> ('a, unit, string, t) format4 -> 'a
  val date: t -> int64
  val id: t -> string
  val message: t -> string
  val string_date: t -> string
end

module type P = sig
  val date: unit -> int64
  val id: unit -> string
  val string_of_date: int64 -> string
end

module Make (P: P) = struct

  type t = {
    date: int64;
    id  : string;
    msg : string;
  }

  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let equal = (=)

  let to_sexp t =
    let open Sexplib.Type in
    List [
      List [ Atom "date"; Atom (Int64.to_string t.date) ];
      List [ Atom "id"  ; Atom t.id ];
      List [ Atom "msg" ; Atom t.msg ];
    ]

  let to_json t =
    `O [
      ("date", `String (Int64.to_string t.date));
      ("id"  , Ezjsonm.encode_string t.id);
      ("msg" , Ezjsonm.encode_string t.msg);
    ]

  let of_json j =
    let date = Ezjsonm.find j ["date"] |> Ezjsonm.get_string |> Int64.of_string in
    let id   = Ezjsonm.find j ["id"]   |> Ezjsonm.decode_string_exn in
    let msg  = Ezjsonm.find j ["msg"]  |> Ezjsonm.decode_string_exn in
    { date; id; msg }

  let write t buf =
    let open Bin_prot.Write in
    Tc.Writer.of_bin_prot
      (bin_write_triple bin_write_network64_int64 bin_write_string bin_write_string)
      (t.date, t.id, t.msg)
      buf

  let read buf =
    let open Bin_prot.Read in
    let date, id, msg =
      Tc.Reader.of_bin_prot
        (bin_read_triple bin_read_network64_int64 bin_read_string bin_read_string)
        buf in
    { date; id; msg }

  let size_of t =
    let open Bin_prot.Size in
    bin_size_triple bin_size_network64_int64 bin_size_string bin_size_string
      (t.date, t.id, t.msg)

  let create ?date ? id fmt =
    let date = match date with
      | None   -> P.date ()
      | Some d -> d in
    let id = match id with
      | None   -> P.id ()
      | Some i -> i in
    ksprintf (fun msg ->
        { date; id; msg }
      ) fmt

  let date t = t.date
  let id t = t.id
  let message t = t.msg
  let string_date t = P.string_of_date t.date
end

module Default = Make (struct

  let date =
    let c = ref 0L in
    fun () -> c := Int64.add !c 1L; !c

  let id =
    let r = string_of_int (Random.int 1024) in
    fun () -> r

  let string_of_date = Int64.to_string

  end)
