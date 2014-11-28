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
  type t
  type head
  type tag
  val create: Ir_config.t -> t Lwt.t
  val fetch: t -> ?depth:int -> uri:string -> tag -> [`Local of head] option Lwt.t
  val push : t -> ?depth:int -> uri:string -> tag -> [`Ok|`Error] Lwt.t
end

module type STORE = sig
  type db
  type head
  type remote
  val uri: string -> remote
  val store: db -> remote
  val fetch: db -> ?depth:int -> remote -> [`Local of head] option Lwt.t
  val pull: db -> ?depth:int -> remote -> [`Merge|`Update] -> unit Ir_merge.result Lwt.t
  val push: db -> ?depth:int -> remote -> [`Ok|`Error] Lwt.t
end


module Make
    (S: Ir_bc.STORE)
    (R: S with type head = S.head and type tag = S.tag) =
struct

  type db = S.t
  type head = S.head

  type remote =
    | Store of db
    | URI of string

  let store db = Store db
  let uri s = URI s

  (* sync objects *)
  let sync ?depth l r =
    S.heads r >>= fun min ->
    S.export l ?depth ~min >>= fun slice ->
    S.import_force r slice

  let fetch t ?depth remote =
    match remote with
    | URI uri ->
      Log.debugf "fetch URI %s" uri;
      begin match S.tag t with
        | None     -> return_none
        | Some tag ->
          R.create (S.config t) >>= fun g ->
          R.fetch g ?depth ~uri tag
      end
    | Store r ->
      Log.debugf "fetch store";
      sync ?depth t r >>= fun () ->
      S.head r >>= function
      | None   -> return_none
      | Some h -> return (Some (`Local h))

  let pull t ?depth remote kind =
    let open Ir_merge.OP in
    fetch t ?depth remote >>= function
    | None -> ok () (* XXX ? *)
    | Some (`Local k) ->
      match kind with
      | `Merge  -> S.merge_head t k
      | `Update ->
        S.update_head t k >>= fun () ->
        ok ()

  let push t ?depth remote =
    Log.debugf "push";
    match remote with
    | URI uri ->
      begin match S.tag t with
        | None     -> return `Error
        | Some tag ->
          R.create (S.config t) >>= fun g ->
          R.push g ?depth ~uri tag
      end
    | Store r ->
      S.head t >>= function
      | None   -> return `Error
      | Some k ->
        sync ?depth r t >>= fun () ->
        S.update_head r k >>= fun () ->
        return `Ok

end

module None (H: Tc.S0) (T: Tc.S0) = struct
  type t = unit
  type head = H.t
  type tag = T.t
  let create _ = return_unit
  let fetch () ?depth:_ ~uri:_ _tag = return_none
  let push () ?depth:_ ~uri:_ _tag = return `Error
end
