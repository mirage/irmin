(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

include IO_intf
open! Import

let src = Logs.Src.create "irmin.pack.io" ~doc:"IO for irmin-pack"

module Log = (val Logs.src_log src : Logs.LOG)

module Cache = struct
  type ('a, 'v) t = { v : 'a -> ?fresh:bool -> ?readonly:bool -> string -> 'v }

  let memoize ~v ~clear ~valid file =
    let files = Hashtbl.create 13 in
    let cached_constructor extra_args ?(fresh = false) ?(readonly = false) root
        =
      let file = file ~root in
      if fresh && readonly then invalid_arg "Read-only IO cannot be fresh";
      try
        if not (Sys.file_exists file) then (
          [%log.debug
            "[%s] does not exist anymore, cleaning up the fd cache"
              (Filename.basename file)];
          Hashtbl.remove files (file, true);
          Hashtbl.remove files (file, false);
          raise Not_found);
        let t = Hashtbl.find files (file, readonly) in
        if valid t then (
          [%log.debug "found in cache: %s (readonly=%b)" file readonly];
          if fresh then clear t;
          t)
        else (
          Hashtbl.remove files (file, readonly);
          raise Not_found)
      with Not_found ->
        [%log.debug
          "[%s] v fresh=%b readonly=%b" (Filename.basename file) fresh readonly];
        let t = v extra_args ~fresh ~readonly file in
        if fresh then clear t;
        Hashtbl.add files (file, readonly) t;
        t
    in
    { v = cached_constructor }
end
