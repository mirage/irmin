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

type classification =
  [ `Branch
  | `Dict
  | `Gc_result of int
  | `Mapping of int
  | `Prefix of int
  | `Reachable of int
  | `Sorted of int
  | `V1_or_v2_pack ]
[@@deriving irmin]

let classify_filename s : classification option =
  match String.split_on_char '.' s with
  | [ "store"; "pack" ] -> Some `V1_or_v2_pack
  | [ "store"; "branches" ] -> Some `Branch
  | [ "store"; "dict" ] -> Some `Dict
  | [ "store"; g; "out" ] when is_number g ->
      Some (`Gc_result (int_of_string g))
  | [ "store"; g; "reachable" ] when is_number g ->
      Some (`Reachable (int_of_string g))
  | [ "store"; g; "sorted" ] when is_number g ->
      Some (`Sorted (int_of_string g))
  | [ "store"; g; "mapping" ] when is_number g ->
      Some (`Mapping (int_of_string g))
  | [ "store"; g; "prefix" ] when is_number g ->
      Some (`Prefix (int_of_string g))
  | _ -> None
