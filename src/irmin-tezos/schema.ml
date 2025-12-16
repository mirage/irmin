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

module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Branch = Irmin.Branch.String

module Hash : Irmin.Hash.S = struct
  module H = Digestif.Make_BLAKE2B (struct
    let digest_size = 32
  end)

  type t = H.t

  let prefix = "\079\199" (* Co(52) *)

  let pp ppf t =
    let s = H.to_raw_string t in
    Tezos_base58.pp ppf (Tezos_base58.encode ~prefix s)

  let of_b58 : string -> (t, [ `Msg of string ]) result =
   fun x ->
    match Tezos_base58.decode ~prefix (Base58 x) with
    | Some x -> Ok (H.of_raw_string x)
    | None -> Error (`Msg "Failed to read b58check_encoding data")

  let short_hash_string = Repr.(unstage (short_hash string))
  let short_hash ?seed t = short_hash_string ?seed (H.to_raw_string t)

  let t : t Repr.t =
    Repr.map ~pp ~of_string:of_b58
      Repr.(string_of (`Fixed H.digest_size))
      ~short_hash H.of_raw_string H.to_raw_string

  let short_hash_string = short_hash_string ?seed:None
  let short_hash t = short_hash_string (H.to_raw_string t)
  let hash_size = H.digest_size

  let short_hash_substring t ~off =
    short_hash_string (Bigstringaf.substring t ~off ~len:hash_size)

  let hash = H.digesti_string
  let to_raw_string = H.to_raw_string
  let unsafe_of_raw_string = H.of_raw_string
end

module Info = Irmin.Info.Default

module Node
    (Contents_key : Irmin.Key.S with type hash = Hash.t)
    (Node_key : Irmin.Key.S with type hash = Hash.t) =
struct
  module M =
    Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata) (Contents_key)
      (Node_key)

  (* [V1] is only used to compute preimage hashes. [assert false]
     statements should be unreachable.*)
  module V1 : sig
    val pre_hash : M.t -> (string -> unit) -> unit
  end = struct
    module Hash = Irmin.Hash.V1 (Hash)

    type entry = string * M.value

    (* Irmin 1.4 uses int8 to store filename lengths.

       Irmin 2 use a variable-size encoding for strings; this is using int8
       for strings of size stricly less than 128 (e.g. 2^7) which happen to
       be the case for all filenames ever produced by Irmin 1.4. *)
    let step_t = Irmin.Type.string

    let metadata_t =
      let some = "\255\000\000\000\000\000\000\000" in
      let none = "\000\000\000\000\000\000\000\000" in
      Irmin.Type.(map (string_of (`Fixed 8)))
        (fun _ -> assert false)
        (function Some _ -> some | None -> none)

    let metadata_of_entry (_, t) =
      match t with `Node _ -> None | `Contents (_, m) -> Some m

    let hash_of_entry (_, t) =
      match t with
      | `Node h -> Node_key.to_hash h
      | `Contents (h, _) -> Contents_key.to_hash h

    (* Irmin 1.4 uses int64 to store list lengths *)
    let entry_t : entry Irmin.Type.t =
      let open Irmin.Type in
      record "Tree.entry" (fun _ _ _ -> assert false)
      |+ field "kind" metadata_t metadata_of_entry
      |+ field "name" step_t fst
      |+ field "hash" Hash.t hash_of_entry
      |> sealr

    let entries_t : entry list Irmin.Type.t =
      Irmin.Type.(list ~len:`Int64 entry_t)

    let pre_hash_entries = Irmin.Type.(unstage (pre_hash entries_t))
    let compare_entry (x, _) (y, _) = String.compare x y
    let step_to_string = Irmin.Type.(unstage (to_bin_string Path.step_t))
    let str_key (k, v) = (step_to_string k, v)

    let pre_hash t =
      M.list t
      |> List.map str_key
      |> List.fast_sort compare_entry
      |> pre_hash_entries
  end

  include M

  let t = Irmin.Type.(like t ~pre_hash:V1.pre_hash)
end

module Commit
    (Node_key : Irmin.Key.S with type hash = Hash.t)
    (Commit_key : Irmin.Key.S with type hash = Hash.t) =
struct
  module M = Irmin.Commit.Generic_key.Make (Hash) (Node_key) (Commit_key)
  module V1 = Irmin.Commit.V1.Make (Hash) (M)
  include M

  let pre_hash_v1_t = Irmin.Type.(unstage (pre_hash V1.t))
  let pre_hash_v1 t = pre_hash_v1_t (V1.import t)
  let t = Irmin.Type.(like t ~pre_hash:pre_hash_v1)
end

module Contents = struct
  type t = bytes

  let ty = Irmin.Type.(pair (bytes_of `Int64) unit)
  let pre_hash_ty = Irmin.Type.(unstage (pre_hash ty))
  let pre_hash_v1 x = pre_hash_ty (x, ())
  let t = Irmin.Type.(like bytes ~pre_hash:pre_hash_v1)
  let merge = Irmin.Merge.(idempotent (Irmin.Type.option t))
end
