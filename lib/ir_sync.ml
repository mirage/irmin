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

module type S = sig
  type db
  type head
  type remote
  val uri: string -> remote
  val store: db -> remote
  val fetch: db -> ?depth:int -> remote -> head option Lwt.t
  val fetch_exn: db -> ?depth:int -> remote -> head Lwt.t
  val push: db -> ?depth:int -> remote -> head option Lwt.t
  val push_exn: db -> ?depth:int -> remote -> head Lwt.t
end

module type REMOTE = functor (S: Ir_bc.STORE) -> sig
  val fetch: S.t -> ?depth:int -> string -> S.head option Lwt.t
  val push : S.t -> ?depth:int -> string -> S.head option Lwt.t
end

module Make (S: Ir_bc.STORE) (R: REMOTE) = struct

  module R = R(S)
  type db = S.t
  type head = S.head

  type remote =
    | Store of db
    | URI of string

  let store db = Store db
  let uri s = URI s

  let sync ?depth l r =
    S.head r >>= function
    | None             -> return_none
    | Some remote_head ->
      S.export l ?depth ~max:[remote_head] >>= fun slice ->
      S.import_force r slice >>= fun () ->
      return (Some remote_head)

  let fetch t ?depth remote =
    match remote with
    | URI uri ->
      Log.debugf "fetch URI %s" uri;
      R.fetch t ?depth uri
    | Store r ->
      Log.debugf "fetch store";
      sync ?depth t r

  let fetch_exn t ?depth remote =
    fetch t ?depth remote >>= function
    | None   -> fail (Failure "fetch")
    | Some d -> return d

  let push t ?depth remote =
    Log.debugf "push";
    match remote with
    | URI uri -> R.push t ?depth uri
    | Store r ->
      sync ?depth r t >>= function
      | None   -> return_none
      | Some k ->
        S.update_head r k >>= fun _ ->
        return (Some k)

  let push_exn t ?depth remote =
    push t ?depth remote >>= function
    | None   -> fail (Failure "push")
    | Some d -> return d

end

module None (S: Ir_bc.STORE) = struct

  let fetch _t ?depth:_ _uri =
    Log.debugf "slow fetch";
    return_none

  let push _t ?depth:_ _uri =
    Log.debugf "slow push";
    return_none

end
