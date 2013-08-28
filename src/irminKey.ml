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

type sha1 = SHA1 of string

module SHA1 = struct

  module T = struct

    type t = sha1

    let compare (SHA1 k1) (SHA1 k2) = String.compare k1 k2

    let hash (SHA1 k) = Hashtbl.hash k

    let equal (SHA1 k1) (SHA1 k2) = String.compare k1 k2 = 0

    let pretty (SHA1 k) =
      Printf.sprintf "%s" (IrminMisc.hex_encode k)

  end

  module Set = IrminMisc.SetMake(T)

  include T

  let to_json (SHA1 k) =
    IrminJSON.of_string k

  let of_json j =
    SHA1 (IrminJSON.to_string j)

  let of_string str =
    SHA1 (IrminMisc.sha1 str)

  let to_hex (SHA1 str) =
    IrminMisc.hex_encode str

  let of_hex hex =
    SHA1 (IrminMisc.hex_decode hex)

  let concat l =
    let l = List.fold_left (fun acc (SHA1 s) -> s :: acc) [] l in
    let s = String.concat "" (List.sort String.compare l) in
    of_string s

  let key_length = 20

  let length (SHA1 _) = key_length

  let sizeof _ = key_length

  let read buf =
    lwt str = IrminIO.get_string buf key_length in
    Lwt.return (SHA1 str)

  let write buf (SHA1 str) =
    IrminIO.set_string buf str

end

module Graph
    (KS: KEY_STORE)
    (VS: VALUE_STORE with type t = KS.t and module Key = KS.Key)
    (TS: TAG_STORE with type t = KS.t and module Key = KS.Key) = struct

  let debug = IrminMisc.debug "GRAPH"
  module Key = KS.Key
  module Value = VS.Value
  module Tag = TS.Tag

  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Key)
  module GO = Graph.Oper.I(G)
  module Topological = Graph.Topological.Make(G)
  include G
  include GO
  let attributes = ref (fun _ -> [])
  let graph_name = ref None
  module Dot = Graph.Graphviz.Dot(struct
      include G
      let edge_attributes _ = []
      let default_edge_attributes _ = []
      let vertex_name k = Key.pretty k
      let vertex_attributes k = !attributes k
      let default_vertex_attributes _ = []
      let get_subgraph _ = None
      let graph_attributes _ =
        match !graph_name with
        | None   -> []
        | Some n -> [`Label n]
    end)

  let dump t g name =
    if IrminMisc.debug_enabled () then (
      lwt tags = TS.all t in
      lwt tags = Lwt_list.fold_left_s (fun tags tag ->
          lwt keys = TS.read t tag in
          let keys = Key.Set.fold (fun key tags ->
              (key, Tag.to_name tag) :: tags
            ) keys [] in
          Lwt.return keys
        ) [] (Tag.Set.to_list tags) in
      let label_tags k =
        let tags = List.fold_left (fun tags (key, tag) ->
            if key = k then tag :: tags else tags
          ) [] tags in
        List.map (fun tag -> `Label tag) tags in
      lwt keys = KS.all t in
      lwt values = Lwt_list.fold_left_s (fun values key ->
          lwt value = VS.read t key in
          match value with
          | None   -> Lwt.return values
          | Some v ->
            match Value.contents v with
            | None   -> Lwt.return values
            | Some k ->
              lwt v = VS.read t k in
              match v with
              | None   -> Lwt.return values
              | Some v ->
                let label = Printf.sprintf "%S" (Value.pretty v) in
                Lwt.return ((key, `Label label) :: values)
        ) [] (Key.Set.to_list keys) in
      let label_value k =
        try [List.assoc k values]
        with Not_found -> [] in
      let attrs k =
        label_tags k @ label_value k in
      attributes := attrs;
      graph_name := Some name;
      Printf.eprintf "%!";
      Dot.output_graph stderr g;
      Printf.eprintf "\n\n%!";
      Lwt.return ()
    ) else
      Lwt.return ()

  let of_store t ?roots ?sinks () =
    let g = G.create () in
    lwt keys = match sinks with
      | None      -> KS.all t
      | Some keys -> Lwt.return keys in
    let marks = Hashtbl.create 1024 in
    let mark key = Hashtbl.add marks key true in
    let has_mark key = Hashtbl.mem marks key in
    let () = match roots with
      | None      -> ()
      | Some keys ->
        Key.Set.iter mark keys;
        Key.Set.iter (G.add_vertex g) keys in
    let rec add key =
      if has_mark key then Lwt.return ()
      else (
        mark key;
        debug "ADD %s" (Key.pretty key);
        if not (G.mem_vertex g key) then G.add_vertex g key;
        lwt keys = KS.pred t key in
        List.iter (fun k -> G.add_edge g k key) (Key.Set.to_list keys);
        Lwt_list.iter_s add (Key.Set.to_list keys)
      ) in
    lwt () = Lwt_list.iter_s add (Key.Set.to_list keys) in
    Lwt.return g

end
