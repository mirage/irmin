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

open Import
include Content_addressable_intf

(** NOTE(craigfe): As of Git 2.1.3, attempting to [reset] repositories
    concurrently can fail due to file-system race conditions. The next version
    should fix this issue, so this global lock is a quick workaround. *)
let reset_lock = Lwt_mutex.create ()

module Make (G : Git.S) (V : Value.S with type value := G.Value.t) = struct
  module H = Irmin.Hash.Make (G.Hash)

  let handle_git_err = function
    | Ok x -> Lwt.return x
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e

  type 'a t = G.t
  type key = H.t
  type value = V.t

  let pp_key = Irmin.Type.pp H.t

  let mem t key =
    Log.debug (fun l -> l "mem %a" pp_key key);
    G.mem t key >>= function
    | false -> Lwt.return_false
    | true -> (
        G.read t key >>= function
        | Error (`Reference_not_found _ | `Not_found _) -> Lwt.return_false
        | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
        | Ok v -> Lwt.return (V.type_eq (G.Value.kind v)))

  let find t key =
    Log.debug (fun l -> l "find %a" pp_key key);
    G.read t key >>= function
    | Error (`Reference_not_found _ | `Not_found _) -> Lwt.return_none
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" G.pp_error e
    | Ok v -> Lwt.return (V.of_git v)

  let add t v =
    let v = V.to_git v in
    let* k, _ = G.write t v >>= handle_git_err in
    Log.debug (fun l -> l "add %a" pp_key k);
    Lwt.return k

  let equal_hash = Irmin.Type.(unstage (equal H.t))

  let unsafe_add t k v =
    let+ k' = add t v in
    if equal_hash k k' then ()
    else
      Fmt.failwith
        "[Git.unsafe_append] %a is not a valid key. Expecting %a instead.\n"
        pp_key k pp_key k'

  let clear t =
    Log.debug (fun l -> l "clear");
    Lwt_mutex.with_lock reset_lock (fun () -> G.reset t) >>= handle_git_err

  let batch t f = f t
  let close _ = Lwt.return ()
end
