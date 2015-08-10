(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module IO = Git_mirage.Sync.IO

module type CONTEXT = sig val v: unit -> IO.ctx option Lwt.t end
module Context (C: CONTEXT) = struct
  type t = IO.ctx
  let v = C.v
end

module Irmin_git = struct
  let config = Irmin_git.config
  let head = Irmin_git.head
  let bare = Irmin_git.bare
  module AO = Irmin_git.AO
  module Memory (C: CONTEXT) = Irmin_git.Memory_ext(Context(C))(IO)
end

module Task (N: sig val name: string end) (C: V1.CLOCK) = struct
  let f msg =
    let date = Int64.of_float (C.time ()) in
    Irmin.Task.create ~date ~owner:N.name msg
end

module KV_RO (C: CONTEXT) (I: Git.Inflate.S) = struct

  open Lwt.Infix

  module S = Irmin.Basic(Irmin_git.Memory(C)(I))(Irmin.Contents.Cstruct)
  module Sync = Irmin.Sync(S)
  let config = Irmin_mem.config ()

  type error = Unknown_key of string
  type 'a io = 'a Lwt.t
  type t = { path: string list; t: S.t; }
  type id
  let disconnect _ = Lwt.return_unit
  type page_aligned_buffer = Cstruct.t
  let unknown_key k = Lwt.return (`Error (Unknown_key k))
  let ok x = Lwt.return (`Ok x)

  let read_head t =
    S.head t.t >>= function
    | None   -> Lwt.return "empty HEAD"
    | Some h ->
      S.task_of_head t.t h >|= fun task ->
      Printf.sprintf
        "commit: %s\n\
         Author: %s\n\
         Date: %Ld\n\
         \n\
         %s\n"
        (S.Head.to_hum h)
        (Irmin.Task.owner task)
        (Irmin.Task.date task)
        (String.concat "\n" @@ Irmin.Task.messages task)

  let mk_path t path =
    let rec aux acc = function
      | []   -> acc
      | h::t -> aux (S.Key.cons (S.Key.Step.of_hum h) acc) t
    in
    aux (S.Key.of_hum path) (List.rev t.path)

  let read_store t path off len =
    S.read t.t (mk_path t path) >>= function
    | None   -> unknown_key path
    | Some v ->
      let buf = Tc.write_cstruct (module S.Val) v in
      let buf = Cstruct.sub buf off len in
      ok [buf]

  let read t path off len = match path with
    | "HEAD" ->
      read_head t >>= fun buf ->
      let buf = Cstruct.sub (Cstruct.of_string buf) off len in
      ok [buf]
    | _ -> read_store t path off len

  let size_head t =
    read_head t >>= fun buf -> ok (Int64.of_int @@ String.length buf)

  let size_store t path =
    S.read t.t (mk_path t path) >>= function
    | None   -> unknown_key path
    | Some v -> ok (Int64.of_int @@ Tc.size_of (module S.Val) v)

  let size t = function
    | "HEAD" -> size_head t
    | path    -> size_store t path

  let connect ?(depth = 1) ?(branch = "master") ?path uri =
    let uri = Irmin.remote_uri (Uri.to_string uri) in
    let tag = S.Tag.of_hum branch in
    let path = match path with
      | None -> []
      | Some s -> List.filter ((<>)"") @@ Stringext.split s ~on:'/'
    in
    S.of_tag config Irmin.Task.none tag >>= fun t ->
    Sync.pull_exn (t ()) ~depth uri `Update >|= fun () ->
    { t = t (); path }

end
