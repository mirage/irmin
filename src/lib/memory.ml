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

open Lwt

module Store (K: API.KEY) (V: API.VALUE) : API.STORE
  with module K = K
   and module V = V
= struct

  module K = K
  module V = V

     type t = {
       store: (K.t, V.t) Hashtbl.t;
       edges: (K.t, K.t) Hashtbl.t;
     }

     let write t value =
       let key = K.create value in
       Hashtbl.add t.store key value;
       return key

     let read t key =
       Printf.printf "Reading %s\n%!" (K.to_string key);
       try return (Some (Hashtbl.find t.store key))
       with Not_found -> return None

     let add_edge t k1 k2 =
       if Hashtbl.mem t.store k1 && Hashtbl.mem t.store k2 then
         Hashtbl.add t.edges k1 k2
       else
         failwith "add_edge"

     let keys t =
       let g = K.Graph.create () in
       Hashtbl.iter (fun k _  -> K.Graph.add_vertex g k) t.store;
       Hashtbl.iter (fun k1 k2 -> K.Graph.add_edge g k1 k2) t.edges;
       g

   end

module Tag_store (T: API.TAG) (K: API.KEY) : API.TAG_STORE
  with module T = T
   and module K = K
= struct

  module T = T
  module K = K

  type t = {
    tags: (T.t, K.t) Hashtbl.t;
  }

  let update t tag key =
    Printf.printf "Update %s to %s\n%!" (T.to_string tag) (K.to_string key);
    Hashtbl.replace t.tags tag key;
    return ()

  let read t tag =
    Printf.printf "Reading %s\n%!" (T.to_string tag);
    try return (Some (Hashtbl.find t.tags tag))
    with Not_found -> return None

  let list t =
    Hashtbl.fold (fun t k acc -> (t, k) :: acc) t.tags []

end
