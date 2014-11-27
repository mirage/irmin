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

module type REMOTE = sig
  val fetch: Ir_config.t -> ?depth:int -> string -> [`Head of string] option Lwt.t
  val push : Ir_config.t -> ?depth:int -> string -> [`Head of string] option Lwt.t
end

module Make (S: Ir_bc.STORE) (H: Tc.S0 with type t = S.head) (R: REMOTE) =
struct

  type db = S.t
  type head = S.head

  let return_head (`Head h) =
    return (Some (Tc.read_string (module H) h))

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
    | Store r ->
      Log.debugf "fetch store";
      sync ?depth t r
    | URI uri ->
      Log.debugf "fetch URI %s" uri;
      R.fetch (S.config t) ?depth uri >>= function
      | None   -> return_none
      | Some h -> return_head h

  let fetch_exn t ?depth remote =
    fetch t ?depth remote >>= function
    | None   -> fail (Failure "fetch")
    | Some d -> return d

  let push t ?depth remote =
    Log.debugf "push";
    match remote with
    | URI uri ->
      begin
        R.push (S.config t) ?depth uri >>= function
        | None   -> return_none
        | Some h -> return_head h
      end
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

module None = struct

  let fetch _t ?depth:_ _uri =
    Log.debugf "slow fetch";
    return_none

  let push _t ?depth:_ _uri =
    Log.debugf "slow push";
    return_none

end
