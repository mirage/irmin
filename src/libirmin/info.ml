(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "info_new"
      (repo @-> string_opt @-> string @-> returning info)
      (fun (type repo) repo author message ->
        with_repo' repo info
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let module Info = Irmin_unix.Info (Store.Info) in
            let info : Info.t = Info.v ?author "%s" message () in
            Root.create_info (module Store) info))

  let () =
    fn "info_update"
      (repo @-> info @-> string_opt @-> string @-> returning void)
      (fun (type repo) repo info author message ->
        with_repo repo ()
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let module Info = Irmin_unix.Info (Store.Info) in
            Root.set_info (module Store) info (Info.v ?author "%s" message ())))

  let () =
    fn "info_message"
      (repo @-> info @-> returning irmin_string)
      (fun (type repo) repo info ->
        with_repo' repo irmin_string
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let info = Root.get_info (module Store) info in
            let s = Store.Info.message info in
            Root.create_string s))

  let () =
    fn "info_author"
      (repo @-> info @-> returning irmin_string)
      (fun (type repo) repo info ->
        with_repo' repo irmin_string
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let info = Root.get_info (module Store) info in
            let s = Store.Info.author info in
            Root.create_string s))

  let () =
    fn "info_date"
      (repo @-> info @-> returning int64_t)
      (fun (type repo) repo info ->
        with_repo repo (-1L)
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let info = Root.get_info (module Store) info in
            Store.Info.date info))

  let () = fn "info_free" (info @-> returning void) free
end
