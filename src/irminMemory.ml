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

module Key_store (C: CORE) = struct

  module C = C
  open C

  module Vertex = struct
    type t = Key.t
    let compare = Key.compare
    let hash = Key.hash
    let equal k1 k2 = Key.compare k1 k2 = 0
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)

  type t = G.t

  let create () =
    G.create ~size:1024 ()

  let add_key g k =
    G.add_vertex g k

  let add_relation g k1 k2 =
    G.add_edge g k1 k2

  let add g key preds =
    add_key g key;
    Key.Set.iter (add_key g) preds;
    Key.Set.iter (fun pred -> add_relation g pred key) preds;
    Lwt.return ()

  let all g =
    let keys = G.fold_vertex (fun k acc -> k :: acc) g [] in
    Lwt.return (Key.Set.of_list keys)

  (* XXX: G.pred is in O(max(|V|,|E|)) *)
  let pred g k =
    try Lwt.return (Key.Set.of_list (G.pred g k))
    with Not_found -> Lwt.return Key.Set.empty

  let succ g k =
    try Lwt.return (Key.Set.of_list (G.succ g k))
    with Not_found -> Lwt.return Key.Set.empty

end

module Value_store (C: CORE) = struct

  module C = C
  open C

  type t = (Key.t, Value.t) Hashtbl.t

  let create () =
    Hashtbl.create 1024

  let write t value =
    let key = Value.key value in
    Hashtbl.add t key value;
    Lwt.return key

  let read t key =
    Printf.printf "Reading %s\n%!" (Key.pretty key);
    try Lwt.return (Some (Hashtbl.find t key))
    with Not_found -> Lwt.return None

end

module Tag_store (C: CORE) = struct

  module C = C
  open C

  type t = (Tag.t, Key.Set.t) Hashtbl.t

  let create () =
    Hashtbl.create 1024

  let update t tag keys =
    Printf.printf "Update %s to %s\n%!" (Tag.pretty tag) (Key.Set.pretty keys);
    if Key.Set.is_empty keys then Hashtbl.remove t tag
    else Hashtbl.replace t tag keys;
    Lwt.return ()

  let remove t tag =
    Hashtbl.remove t tag;
    Lwt.return ()

  let read t tag =
    Printf.printf "Reading %s\n%!" (Tag.pretty tag);
    try Lwt.return (Hashtbl.find t tag)
    with Not_found -> Lwt.return Key.Set.empty

  let all t =
    let elts = Hashtbl.fold (fun t _ acc -> t :: acc) t [] in
    Lwt.return (Tag.Set.of_list elts)

end

module type S = sig
  include STORE
  val create: unit -> t
end

module Make (C: CORE) = struct
  module C = C
  module Key_store = Key_store(C)
  module Value_store = Value_store(C)
  module Tag_store = Tag_store(C)
  type t = {
    k: Key_store.t;
    v: Value_store.t;
    t: Tag_store.t;
  }
  let key_store t = t.k
  let value_store t = t.v
  let tag_store t = t.t
  let create () = {
    k = Key_store.create ();
    v = Value_store.create ();
    t = Tag_store.create ();
  }
end
