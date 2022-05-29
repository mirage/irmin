(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

open! Import
open Io_intf

module type S = S

module Unix = struct
  type fixme = unit
  type misc_error = Unix.error * string * string
  type create_error = [ `Io_misc of misc_error ]

  type open_error =
    [ `Io_misc of misc_error | `No_such_file_or_directory | `Is_a_directory ]

  type read_error =
    [ `Io_misc of misc_error | `Read_out_of_bounds | `Read_on_closed ]

  type write_error =
    [ `Io_misc of misc_error | `Ro_not_allowed | `Write_on_closed ]

  type close_error = [ `Io_misc of misc_error | `Double_close ]

  type reload_error =
    [ `Io_misc of misc_error | `Rw_not_allowed | `Reload_on_closed ]

  type mkdir_error =
    [ `Io_misc of misc_error | `File_exists | `No_such_file_or_directory ]

  type move_file_error = [ `Io_misc of misc_error ]
  type t = fixme

  exception Read_error of read_error
  exception Write_error of write_error

  let create ~path ~overwrite =
    ignore (path, overwrite);
    Ok ()

  let open_ ~path ~readonly =
    ignore (path, readonly);
    Ok ()

  let close t =
    ignore t;
    Ok ()

  let write_string t ~off s =
    ignore (t, off, s);
    Ok ()

  let fsync t =
    ignore t;
    Ok ()

  let read_to_string t ~off ~len =
    ignore (t, off, len);
    Ok ""

  let read_exn t ~off ~len s =
    ignore (t, off, len, s);
    ()

  let write_exn t ~off s =
    ignore (t, off, s);
    ()

  let size _ = assert false

  (* let size _ = Int63.zero
   * let reload _ = assert false *)

  let readonly t =
    ignore t;
    true

  let path t =
    ignore t;
    ""

  let classify_path _ = assert false
  let page_size = 4096
  let move_file ~src:_ ~dst:_ = assert false
  let mkdir _ = assert false
end
