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

(** Store module types + [Json_tree] helper.

    The implementation functor [Store.Make (B : Backend.S)] (~1300 lines,
    verbatim from [main]) used to live here, alongside [Json_tree] and the
    [Remote.t Store] constructor. [Store.Make] has been removed: all backends in
    [irmin-lwt] now go through [Wrap_store.Make] (which delegates to Irmin 4's
    [Of_backend] internally). [Of_backend] and [Generic_key.Maker] -- the only
    consumers of [Store.Make] -- are also no longer exposed (see
    LIMITATIONS.md). [Json_tree] is kept because it operates on any [Store.S]
    regardless of how it was built. *)

open! Import
include Store_intf

module Generic_key = struct
  module type S = S_generic_key
  module type KV = KV_generic_key
  module type Maker = Maker_generic_key
  module type KV_maker = KV_maker_generic_key
end

module Json_tree (Store : S with type Schema.Contents.t = Contents.json) =
struct
  include Contents.Json_value

  type json = Contents.json

  let to_concrete_tree j : Store.Tree.concrete =
    let rec obj j acc =
      match j with
      | [] -> `Tree acc
      | (k, v) :: l -> (
          match Type.of_string Store.Path.step_t k with
          | Ok key -> obj l ((key, node v []) :: acc)
          | _ -> obj l acc)
    and node j acc =
      match j with
      | `O j -> obj j acc
      | _ -> `Contents (j, Store.Metadata.default)
    in
    node j []

  let of_concrete_tree c : json =
    let step = Type.to_string Store.Path.step_t in
    let rec tree t acc =
      match t with
      | [] -> `O acc
      | (k, v) :: l -> tree l ((step k, contents v []) :: acc)
    and contents t acc =
      match t with `Contents (c, _) -> c | `Tree c -> tree c acc
    in
    contents c []

  let set_tree (tree : Store.tree) key j : Store.tree Lwt.t =
    let c = to_concrete_tree j in
    let c = Store.Tree.of_concrete c in
    Store.Tree.add_tree tree key c

  let get_tree (tree : Store.tree) key =
    let* t = Store.Tree.get_tree tree key in
    let+ c = Store.Tree.to_concrete t in
    of_concrete_tree c

  let set t key j ~info =
    set_tree (Store.Tree.empty ()) Store.Path.empty j >>= function
    | tree -> Store.set_tree_exn ~info t key tree

  let get t key =
    let* tree = Store.get_tree t key in
    get_tree tree Store.Path.empty
end

type Remote.t +=
  | Store : (module Generic_key.S with type t = 'a) * 'a -> Remote.t
