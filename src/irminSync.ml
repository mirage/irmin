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

open IrminTypes

module Event (C: CORE) = struct

  module C = C

  open C

  module T = struct

    type t =
      | Tag of (Tag.t * Key.Set.t)   (** A tag has been updated. *)
      | Graph of Key.Graph.t          (** New keys have been added. *)

    let compare t1 t2 =
      match t1, t2 with
      | Tag  _ , Graph _ -> 1
      | Graph _, Tag _   -> -1
      | Tag _  , Tag _   -> failwith "TODO"
      | Graph _, Graph _ -> failwith "TODO"

    let pretty = function
      | Tag (t,ks) -> Printf.sprintf "%s:%s" (Tag.pretty t) (Key.Set.pretty ks)
      | Graph k    -> Printf.sprintf "%s" (Key.Set.pretty (Key.Graph.vertex k))

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash = Hashtbl.hash

    let get buf =
      match IrminIO.get_uint8 buf with
      | 0 ->
        let tag = Tag.get buf in
        let keys = Key.Set.get buf in
        Tag (tag, keys)
      | 1 ->
        let g = Key.Graph.get buf in
        Graph g
      | _ -> failwith "Event.get"

    let set buf t =
      match t with
      | Tag (tag, keys) ->
        IrminIO.set_uint8 buf 0;
        Tag.set buf tag;
        Key.Set.set buf keys
      | Graph g ->
        IrminIO.set_uint8 buf 1;
        Key.Graph.set buf g

    let sizeof = function
      | Tag (tag, keys) -> 1 + Tag.sizeof tag + Key.Set.sizeof keys
      | Graph g         -> 1 + Key.Graph.sizeof g

    let to_json _ =
      failwith "TODO"

    let of_json _ =
      failwith "TODO"

  end

  module Set = IrminContainer.Set(T)

  include T

end


module Make (S: STORE) = struct

  module C = S.C

  module Event = Event(C)

  type t = unit

  let pull_keys _ =
    failwith "TODO"

  let pull_tags _ =
    failwith "TODO"

  let watch _ =
    failwith "TODO"

end
