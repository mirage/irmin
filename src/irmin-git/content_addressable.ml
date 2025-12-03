(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Lwt.Infix
open Lwt.Syntax
include Content_addressable_intf

module Make (G : Git.S) (V : Value.S with type value := G.Value.t) = struct
  module H = Irmin.Hash.Make (G.Hash)

  let handle_git_err = function
    | Ok x -> Lwt.return x
    | Error e -> Fmt.kstr Lwt.fail_with "%a" G.pp_error e

  type 'a t = G.t
  type key = H.t [@@deriving irmin ~pp ~equal]
  type value = V.t

  let mem t key =
    Lwt_eio.run_lwt @@ fun () ->
    [%log.debug "mem %a" pp_key key];
    G.mem t key >>= function
    | false -> Lwt.return_false
    | true -> (
        G.read t key >>= function
        | Error (`Reference_not_found _ | `Not_found _) -> Lwt.return_false
        | Error e -> Fmt.kstr Lwt.fail_with "%a" G.pp_error e
        | Ok v -> Lwt.return (V.type_eq (G.Value.kind v)))

  let find t key =
    Lwt_eio.run_lwt @@ fun () ->
    [%log.debug "find %a" pp_key key];
    G.read t key >>= function
    | Error (`Reference_not_found _ | `Not_found _) -> Lwt.return_none
    | Error e -> Fmt.kstr Lwt.fail_with "%a" G.pp_error e
    | Ok v -> Lwt.return (V.of_git v)

  let add t v =
    Lwt_eio.run_lwt @@ fun () ->
    let v = V.to_git v in
    let* k, _ = G.write t v >>= handle_git_err in
    [%log.debug "add %a" pp_key k];
    Lwt.return k

  let unsafe_add t k v =
    let k' = add t v in
    if equal_key k k' then ()
    else
      Fmt.failwith
        "[Git.unsafe_append] %a is not a valid key. Expecting %a instead.\n"
        pp_key k pp_key k'

  let batch t f = f t
  let close _ = ()
end

module Check_closed (S : Irmin.Content_addressable.S) = struct
  type 'a t = bool ref * 'a S.t
  type key = S.key
  type value = S.value

  let check_not_closed t = if !(fst t) then raise Irmin.Closed

  let mem t k =
    check_not_closed t;
    S.mem (snd t) k

  let find t k =
    check_not_closed t;
    S.find (snd t) k

  let add t v =
    check_not_closed t;
    S.add (snd t) v

  let unsafe_add t k v =
    check_not_closed t;
    S.unsafe_add (snd t) k v

  let batch t f =
    check_not_closed t;
    S.batch (snd t) (fun x -> f (fst t, x))

  let close (c, _) = c := true
end
