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

module Key_store (K: KEY) = struct

  module Key = K

  module Vertex = struct
    type t = K.t
    let compare = K.compare
    let hash = K.hash
    let equal k1 k2 = K.compare k1 k2 = 0
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)

  type t = G.t

  let create () =
    G.create ~size:1024 ()

  let add_key g k =
    G.add_vertex g k;
    Lwt.return ()

  let add_relation g k1 k2 =
    G.add_edge g k1 k2;
    Lwt.return ()

  let list g =
    let keys = G.fold_vertex (fun k acc -> k :: acc) g [] in
    Lwt.return keys

  (* XXX: G.pred is in O(max(|V|,|E|)) *)
  let pred g k =
    try Lwt.return (G.pred g k)
    with Not_found -> Lwt.return []

  let succ g k =
    try Lwt.return (G.succ g k)
    with Not_found -> Lwt.return []

end

module Value_store (K: KEY) (V: VALUE with module Key = K) = struct

  module Key = K

  module Value = V

  type t = (K.t, V.t) Hashtbl.t

  let create () =
    Hashtbl.create 1024

  let write t value =
    let key = V.key value in
    Hashtbl.add t key value;
    Lwt.return key

  let read t key =
    Printf.printf "Reading %s\n%!" (K.pretty key);
    try Lwt.return (Some (Hashtbl.find t key))
    with Not_found -> Lwt.return None

end

module Tag_store (T: TAG) (K: KEY) = struct

  module Tag = T

  module Key = K

  type t = (T.t, K.t) Hashtbl.t

  let create () =
    Hashtbl.create 1024

  let update t tag key =
    Printf.printf "Update %s to %s\n%!" (T.pretty tag) (K.pretty key);
    Hashtbl.replace t tag key;
    Lwt.return ()

  let remove t tag =
    Hashtbl.remove t tag;
    Lwt.return ()

  let read t tag =
    Printf.printf "Reading %s\n%!" (T.pretty tag);
    try Lwt.return (Some (Hashtbl.find t tag))
    with Not_found -> Lwt.return None

  let list t =
    let elts = Hashtbl.fold (fun t _ acc -> t :: acc) t [] in
    Lwt.return elts

end
