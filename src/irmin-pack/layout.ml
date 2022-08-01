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

let toplevel name ~root = Filename.(concat root name)

module V1_and_v2 = struct
  let pack = toplevel "store.pack"
  let branch = toplevel "store.branches"
  let dict = toplevel "store.dict"
  let all ~root = [ pack ~root; branch ~root; dict ~root ]
end

module V3 = struct
  let branch = toplevel "store.branches"
  let dict = toplevel "store.dict"
  let control = toplevel "store.control"

  let suffix ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".suffix")

  let gc_result ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".out")

  let reachable ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".reachable")

  let sorted ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".sorted")

  let mapping =
    (* if we use mmap for the mapping file, then the mapping file is not portable between
       architectures with different endianness; to avoid the possibility that a
       little-endian file is used on a big-endian arch (or vice versa) we use a filename
       that is not portable between archs; since the common case is little endian, we make
       this the "default" empty suffix, and for big endian we add ".be" *)
    let empty_or_be = match Sys.big_endian with false -> "" | true -> ".be" in
    fun ~generation ->
      toplevel ("store." ^ string_of_int generation ^ ".mapping" ^ empty_or_be)

  let prefix ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".prefix")

  let all ~generation ~root =
    [ suffix ~generation ~root; branch ~root; dict ~root; control ~root ]
end
