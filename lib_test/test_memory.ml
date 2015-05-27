(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Test_common
let (>>=) = Lwt.(>>=)

let clean config (module S: Irmin.S) () =
  S.empty config Irmin.Task.none  >>= fun t ->
  S.tags (t ()) >>= fun tags ->
  Lwt_list.iter_p (S.remove_tag (t ())) tags

let suite k =
  let config = Irmin_mem.config () in
  let store = mem_store k in
  {
    name   = "MEM" ^ string_of_contents k;
    kind   = `Mem;
    cont   = k;
    init   = none;
    clean  = clean config store;
    config; store;
}
