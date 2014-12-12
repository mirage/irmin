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

open Lwt

module Log = Log.Make(struct let section = "SYNC" end)

module type STORE = sig
  type db
  type head
  type remote
  val uri: string -> remote
  val store: (module Ir_s.STORE with type t = 'a) -> 'a -> remote
  val fetch: db -> ?depth:int -> remote -> head option Lwt.t
  val fetch_exn: db -> ?depth:int -> remote -> head Lwt.t
  val pull: db -> ?depth:int -> remote -> [`Merge|`Update] -> unit Ir_merge.result Lwt.t
  val pull_exn: db -> ?depth:int -> remote -> [`Merge|`Update] -> unit Lwt.t
  val push: db -> ?depth:int -> remote -> [`Ok | `Error] Lwt.t
  val push_exn: db -> ?depth:int -> remote -> unit Lwt.t
end

module Make (S: Ir_s.STORE) = struct

  module B = S.Private.Sync
  type db = S.t
  type head = S.head

  type remote =
    | Store: (module Ir_s.STORE with type t = 'a) * 'a -> remote
    | URI of string

  let store m x = Store (m, x)
  let uri s = URI s

  (* sync objects *)
  let sync (type s) (type r)
      (module S: Ir_s.STORE with type t = s)
      (module R: Ir_s.STORE with type t = r)
      ?depth l (r:r)
    =
    R.heads r >>= fun min ->
    let min = List.map (fun r -> S.Head.of_raw (R.Head.to_raw r)) min in
    S.export l ?depth ~min >>= fun slice ->
    (* FIXME: the serialiaztion of slices is broken
      let p_slice = Tc.write_cstruct (module S.Private.Slice) slice in
      let slice = Tc.read_cstruct (module R.Private.Slice) p_slice in
    *)
    let p_slice = Tc.to_json (module S.Private.Slice) slice in
    let slice = Tc.of_json (module R.Private.Slice) p_slice in
    R.import_force r slice

  let fetch t ?depth remote =
    match remote with
    | URI uri ->
      Log.debugf "fetch URI %s" uri;
      begin match S.tag t with
        | None     -> return_none
        | Some tag ->
          B.create (S.config t) >>= fun g ->
          B.fetch g ?depth ~uri tag >>= function
          | None  -> return_none
          | Some (`Local h) -> return (Some h)
      end
    | Store ((module R), r) ->
      Log.debugf "fetch store";
      sync (module S) (module R) ?depth t r >>= fun () ->
      R.head r >>= function
      | None   -> return_none
      | Some h ->
        let h = S.Head.of_raw (R.Head.to_raw h) in
        return (Some h)

  let fetch_exn t ?depth remote =
    fetch t ?depth remote >>= function
    | Some h -> return h
    | None   -> fail (Failure "Sync.fetch_exn")

  let pull t ?depth remote kind =
    let open Ir_merge.OP in
    fetch t ?depth remote >>= function
    | None -> ok () (* XXX ? *)
    | Some k ->
      match kind with
      | `Merge  -> S.merge_head t k
      | `Update ->
        S.update_head t k >>= fun () ->
        ok ()

  let pull_exn t ?depth remote kind =
    pull t ?depth remote kind >>=
    Ir_merge.exn

  let push t ?depth remote =
    Log.debugf "push";
    match remote with
    | URI uri ->
      begin match S.tag t with
        | None     -> return `Error
        | Some tag ->
          B.create (S.config t) >>= fun g ->
          B.push g ?depth ~uri tag
      end
    | Store ((module R), r) ->
      S.head t >>= function
      | None   -> return `Error
      | Some h ->
        sync (module R) (module S) ?depth r t >>= fun () ->
        let h = R.Head.of_raw (S.Head.to_raw h) in
        R.update_head r h >>= fun () ->
        return `Ok

  let push_exn t ?depth remote =
    push t ?depth remote >>= function
    | `Ok    -> return_unit
    | `Error -> fail (Failure "Sync.push_exn")

end
