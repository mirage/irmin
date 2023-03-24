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

  let mapping ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".mapping")

  let prefix ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".prefix")

  let all ~generation ~root =
    [ suffix ~generation ~root; branch ~root; dict ~root; control ~root ]
end

module V4 = struct
  let branch = toplevel "store.branches"
  let dict = toplevel "store.dict"
  let control = toplevel "store.control"
  let control_tmp = toplevel "store.control.tmp"

  let suffix_chunk ~chunk_idx =
    toplevel ("store." ^ string_of_int chunk_idx ^ ".suffix")

  let gc_result ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".out")

  let reachable ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".reachable")

  let sorted ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".sorted")

  let mapping ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".mapping")

  let prefix ~generation =
    toplevel ("store." ^ string_of_int generation ^ ".prefix")
end

module V5 = struct
  include V4

  module Volume = struct
    let directory ~idx = toplevel ("volume." ^ string_of_int idx)
    let control = toplevel "volume.control"

    let control_gc_tmp ~generation =
      toplevel ("volume." ^ string_of_int generation ^ ".control")

    let mapping = toplevel "volume.mapping"
    let data = toplevel "volume.data"
  end
end

(** [is_number] is a less generic than [Stdlib.int_of_string_opt]. It matches
    this equivalent regex: {v "([1-9][0-9]*|0)" v}. *)
let is_number s =
  match String.to_seq s |> List.of_seq with
  | [] -> false
  | '0' :: _ :: _ -> false
  | l ->
      List.fold_left
        (fun acc char ->
          let is_digit = match char with '0' .. '9' -> true | _ -> false in
          acc && is_digit)
        true l

module Classification = struct
  module Upper = struct
    type t =
      [ `Branch
      | `Control
      | `Control_tmp
      | `Dict
      | `Gc_result of int
      | `Mapping of int
      | `Prefix of int
      | `Reachable of int
      | `Sorted of int
      | `Suffix of int
      | `V1_or_v2_pack
      | `Unknown ]
    [@@deriving irmin]

    let v s : t =
      match String.split_on_char '.' s with
      | [ "store"; "pack" ] -> `V1_or_v2_pack
      | [ "store"; "branches" ] -> `Branch
      | [ "store"; "control" ] -> `Control
      | [ "store"; "control"; "tmp" ] -> `Control_tmp
      | [ "store"; "dict" ] -> `Dict
      | [ "store"; g; "out" ] when is_number g -> `Gc_result (int_of_string g)
      | [ "store"; g; "reachable" ] when is_number g ->
          `Reachable (int_of_string g)
      | [ "store"; g; "sorted" ] when is_number g -> `Sorted (int_of_string g)
      | [ "store"; g; "mapping" ] when is_number g -> `Mapping (int_of_string g)
      | [ "store"; g; "prefix" ] when is_number g -> `Prefix (int_of_string g)
      | [ "store"; g; "suffix" ] when is_number g -> `Suffix (int_of_string g)
      | _ -> `Unknown
  end

  module Volume = struct
    type t = [ `Mapping | `Data | `Control | `Control_tmp of int | `Unknown ]
    [@@deriving irmin]

    let v s : t =
      match String.split_on_char '.' s with
      | [ "volume"; "control" ] -> `Control
      | [ "volume"; g; "control" ] when is_number g ->
          `Control_tmp (int_of_string g)
      | [ "volume"; "mapping" ] -> `Mapping
      | [ "volume"; "data" ] -> `Data
      | _ -> `Unknown
  end
end
