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

type ('head, 'tag, 'origin, 'slice) store =
  (module Ir_bc.STORE with type head = 'head
                       and type origin = 'origin
                       and type tag = 'tag
                       and type slice = 'slice)

type ('head, 'origin, 'slice) remote =
  | Store: ('head, 'tag, 'origin, 'slice) store * 'tag
    -> ('head, 'origin, 'slice) remote
  | URI of string

let store m b = Store (m, b)

let uri s = URI s

module type STORE = sig
  type t
  type head
  type origin
  type slice
  val fetch: t -> origin -> ?depth:int -> (head, origin, slice) remote -> head option Lwt.t
  val fetch_exn: t -> origin -> ?depth:int -> (head, origin, slice) remote -> head Lwt.t
  val push: t -> origin -> ?depth:int -> (head, origin, slice) remote -> head option Lwt.t
  val push_exn: t -> origin -> ?depth:int -> (head, origin, slice) remote -> head Lwt.t
end

module type REMOTE = sig
  type head
  val fetch: ?depth:int -> string -> head option Lwt.t
  val push : ?depth:int -> string -> head option Lwt.t
end

module Fast (S: Ir_bc.STORE) (R: REMOTE with type head = S.head) = struct

  type t = S.t
  type head = S.head
  type origin = S.origin
  type slice = S.slice

  let sync (type l) (type r) (type h) (type o) (type s)
      ?depth
      (module L: Ir_bc.STORE with type t = l
                              and type head = h
                              and type origin = o
                              and type slice = s)
      (l:l)
      (module R: Ir_bc.STORE with type t = r
                              and type head = h
                              and type origin = o
                              and type slice = s)
      (r:r)
      origin
    =
    R.head r origin >>= function
    | None             -> return_none
    | Some remote_head ->
      L.export l origin ?depth ~max:[remote_head] >>= fun slice ->
      R.import_force r origin slice >>= fun () ->
      return (Some remote_head)

  let fetch t origin ?depth (remote: (S.head, S.origin, S.slice) remote) =
    match remote with
    | URI uri ->
      Log.debugf "fetch URI %s" uri;
      R.fetch ?depth uri
    | Store ((module R), tag) ->
      Log.debugf "fetch store";
      let r = R.of_tag tag in
      sync ?depth (module S) t (module R) r origin

  let fetch_exn t origin ?depth remote =
    fetch t origin ?depth remote >>= function
    | None   -> fail (Failure "fetch")
    | Some d -> return d

  let push t origin ?depth (remote: (S.head, S.origin, S.slice) remote) =
    Log.debugf "push";
    match remote with
    | URI uri -> R.push ?depth uri
    | Store ((module R), tag) ->
      let r = R.of_tag tag in
      sync ?depth (module R) r (module S) t origin >>= function
      | None   -> return_none
      | Some k ->
        R.update_head r origin k >>= fun _ ->
        return (Some k)

  let push_exn t origin ?depth remote =
    push t origin ?depth remote >>= function
    | None   -> fail (Failure "push")
    | Some d -> return d

end

module Slow (S: Ir_bc.STORE) = struct
  module B = struct

    type head = S.head

    let fetch ?depth:_ _uri =
      Log.debugf "slow fetch";
      return_none

    let push ?depth:_ _uri =
      Log.debugf "slow push";
      return_none

  end
  include Fast(S)(B)
end
