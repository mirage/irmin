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

module Types = struct

  type key = K of string

  type blob = B of string

  type revision = {
    parents : key list;
    contents: key;
  }

  type value =
    | Blob of blob
    | Revision of revision

  type local_tag = L of string

  type remote_tag = R of string

end

open Lwt
open Types

module Channel: IrminAPI.CHANNEL
  with type t = Lwt_unix.file_descr
= struct

  (* From https://github.com/djs55/ocaml-vnc/blob/master/lib/rfb_lwt.ml *)
  type t = Lwt_unix.file_descr

  let read_string fd n =
    let buf = String.make n '\000' in
    let rec rread fd buf ofs len =
      lwt n = Lwt_unix.read fd buf ofs len in
      if n = 0 then raise End_of_file;
      if n < len then rread fd buf (ofs + n) (len - n) else return () in
    lwt () = rread fd buf 0 n in
    return buf

  let write_string fd buf =
    let rec rwrite fd buf ofs len =
      lwt n = Lwt_unix.write fd buf ofs len in
      if n = 0 then raise End_of_file;
      if n < len then rwrite fd buf (ofs + n) (len - n) else return () in
    lwt () = rwrite fd buf 0 (String.length buf) in
    return ()

  let read fd n =
    lwt result = read_string fd n in
    return (Cstruct.of_string result)

  let write fd buf =
    write_string fd (Cstruct.to_string buf)

end

module type StringArg = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

module StringBase(A: StringArg): IrminAPI.BASE
  with type t = A.t
   and type channel = Channel.t
= struct

  type t = A.t

  type channel = Channel.t

  let to_string s =
    Printf.sprintf "%S" (A.to_string s)

  let to_json t =
    IrminJSON.of_string (A.to_string t)

  let of_json j =
    A.of_string (IrminJSON.to_string j)

  cstruct hdr {
      uint32_t length
    } as big_endian

  let read fd =
    lwt buf = Channel.read fd sizeof_hdr in
    let len = Int32.to_int (get_hdr_length buf) in
    lwt str = Channel.read_string fd len in
    return (A.of_string str)

  let write fd t =
    let str = A.to_string t in
    let len = String.length str in
    let buf = Cstruct.create sizeof_hdr in
    set_hdr_length buf (Int32.of_int len);
    lwt () = Channel.write fd buf in
    Channel.write_string fd str

  let reads fd =
    lwt buf = Channel.read fd sizeof_hdr in
    let len = Int32.to_int (get_hdr_length buf) in
    let rec aux acc i =
      if i <= 0 then return (List.rev acc)
      else
        lwt t = read fd in
        aux (t :: acc) (i-1) in
    aux [] len

  let writes fd ts =
    let len = List.length ts in
    let buf = Cstruct.create sizeof_hdr in
    set_hdr_length buf (Int32.of_int len);
    Lwt_list.iter_s (write fd) ts

end

module type IterArg = sig
  type t
  val read: Channel.t -> t Lwt.t
  val write: Channel.t -> t -> unit Lwt.t
end

module Iter(A: IterArg) = struct

  cstruct hdr {
      uint32_t keys
    } as big_endian

  let reads fd =
    lwt buf = Channel.read fd sizeof_hdr in
    let keys = Int32.to_int (get_hdr_keys buf) in
    let rec aux acc i =
      if i <= 0 then return (List.rev acc)
      else
        lwt t = A.read fd in
        aux (t :: acc) (i-1) in
    aux [] keys

  let writes fd ts =
    Lwt_list.iter_s (A.write fd) ts

end

module Blob: IrminAPI.BASE
  with type t = blob
   and type channel = Channel.t
= struct

  module S = StringBase(struct
      type t = blob
      let to_string (B s) = s
      let of_string s = B s
    end)

  include S

end

module Key: IrminAPI.KEY
  with type t = key
   and type channel = Channel.t
= struct

  type t = key

  type channel = Channel.t

  let to_string (K k) =
    Printf.sprintf "%s" k

  let to_json (K k) =
    IrminJSON.of_string k

  let of_json j =
    K (IrminJSON.to_string j)

  let create value =
    let str = Marshal.to_string value [] in
    K (IrminMisc.sha1 str)

  let key_length = 20

  let length (K _) = key_length

  let read fd =
    lwt key = Channel.read_string fd key_length in
    return (K key)

  let write fd (K k) =
    Channel.write_string fd k

  include Iter(struct
      type t = key
      let read = read
      let write = write
    end)

  module Vertex = struct
    type t = key
    let compare (K k1) (K k2) = String.compare k1 k2
    let hash (K k) = Hashtbl.hash k
    let equal (K k1) (K k2) = String.compare k1 k2 = 0
  end
  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)

  module Graph = struct

    type channel = Channel.t

    include G

    let vertex g =
      G.fold_vertex (fun v acc -> v :: acc) g []

    let edges g =
      G.fold_edges (fun v1 v2 acc -> (v1, v2) :: acc) g []

    let to_string g =
      let buf = Buffer.create 124 in
      Printf.bprintf buf "digraph {\n";
      G.iter_vertex (fun v ->
          Printf.bprintf buf "%S;\n" (to_string v)
        ) g;
      G.iter_edges (fun v1 v2 ->
          Printf.bprintf buf "%S -> %S;\n" (to_string v1) (to_string v2)
        ) g;
      Printf.bprintf buf "}\n";
      Buffer.contents buf

    let to_json g =
      `O [ ("vertex", IrminJSON.of_list to_json (vertex g));
           ("edges" , IrminJSON.of_list (IrminJSON.of_pair to_json to_json) (edges g)) ]

    let of_json = function
      | `O l ->
        let vertex =
          try IrminJSON.to_list of_json (List.assoc "vertex" l)
          with Not_found -> failwith "Key.Graph.of_json (missing 'vertex')" in
        let edges =
          try IrminJSON.to_list
                (IrminJSON.to_pair of_json of_json) (List.assoc "edges" l)
          with Not_found -> [] in
        let g = G.create ~size:(List.length vertex) () in
        List.iter (G.add_vertex g) vertex;
        List.iter (fun (v1,v2) -> G.add_edge g v1 v2) edges;
        g
      | _ -> failwith "Key.Graph.of_json (not an object)"

    cstruct hdr {
        uint32_t vertex;
        uint32_t edges
      } as big_endian

    let read fd =
    lwt buf = Channel.read fd sizeof_hdr in
    let v = Int32.to_int (get_hdr_vertex buf) in
    let e = Int32.to_int (get_hdr_edges buf) in
    let g = G.create ~size:v () in
    let rec vertex_f i =
      if i <= 0 then return ()
      else
        lwt key = Channel.read_string fd key_length in
        G.add_vertex g (K key);
        vertex_f (i-1) in
    let rec edges_f i =
      if i <= 0 then return ()
      else
        lwt key1 = Channel.read_string fd key_length in
        lwt key2 = Channel.read_string fd key_length in
        G.add_edge g (K key1) (K key2);
        edges_f (i-1) in
    lwt () = vertex_f v in
    lwt () = edges_f e in
    return g

    let reads _ =
      failwith "TODO"

    let write fd g =
      let vertex = vertex g in
      let edges = edges g in
      let v = List.length vertex in
      let e = List.length edges in
      let buf = Cstruct.create sizeof_hdr in
      set_hdr_vertex buf (Int32.of_int v);
      set_hdr_edges buf (Int32.of_int e);
      lwt () = Channel.write fd buf in
      let rec vertex_f = function
        | []       -> return ()
        | (K k)::t ->
          lwt () = Channel.write_string fd k in
          vertex_f t in
      let rec edges_f = function
        | []              -> return ()
        | (K k1, K k2)::t ->
          lwt () = Channel.write_string fd k1 in
          lwt () = Channel.write_string fd k2 in
          edges_f t in
      lwt () = vertex_f vertex in
      edges_f edges

    let writes _ =
      failwith "TODO"

  end

  type graph = Graph.t

end

module Revision: IrminAPI.BASE
  with type t = revision
   and type channel = Channel.t = struct

  type t = revision

  type channel = Channel.t

  let to_string r =
    Printf.sprintf "[%s => %s]"
      (String.concat "-" (List.map Key.to_string r.parents))
      (Key.to_string r.contents)

  let of_json (json:IrminJSON.t) = match json with
    | `O [ ("parents", parents); ("contents", contents) ] ->
      let parents = IrminJSON.to_list Key.of_json parents in
      let parents = List.sort compare parents in
      let contents = Key.of_json contents in
      { parents; contents }
    | _ -> failwith "Revision.of_json"

  let to_json r =
    let parents = IrminJSON.of_list Key.to_json r.parents in
    let contents = Key.to_json r.contents in
    `O [ ("parents", parents); ("contents", contents) ]

  let read fd =
    lwt keys = Key.reads fd in
    match keys with
    | []   -> failwith "Revision.read"
    | h::t -> return { contents = h; parents = t }

  let write fd r =
    Key.writes fd (r.contents :: r.parents)

  let reads _ =
    failwith "TODO"

  let writes _ =
    failwith "TODO"

end

module Value: IrminAPI.BASE
  with type t = value
   and type channel = Channel.t
= struct

  type t = value

  type channel = Channel.t

  let to_string = function
    | Blob b     -> Blob.to_string b
    | Revision r -> Revision.to_string r

  let to_json = function
    | Blob b     -> `O [ "blob"    , Blob.to_json b ]
    | Revision r -> `O [ "revision", Revision.to_json r ]

  let of_json = function
    | `O [ "blob"    , json ] -> Blob (Blob.of_json json)
    | `O [ "revision", json ] -> Revision (Revision.of_json json)
    | _ -> failwith "value_of_json"

  cstruct hdr {
      uint8_t kind
    } as big_endian

  let read fd =
    lwt buf = Channel.read fd sizeof_hdr in
    let kind = get_hdr_kind buf in
    match kind with
    | 0 -> lwt b = Blob.read fd in return (Blob b)
    | 1 -> lwt r = Revision.read fd in return (Revision r)
    | _ -> failwith "Value.of_cstruct"

  let write fd t =
    let kind, cont = match t with
      | Blob b     -> 0, fun () -> Blob.write fd b
      | Revision r -> 1, fun () -> Revision.write fd r in
    let buf = Cstruct.create sizeof_hdr in
    set_hdr_kind buf kind;
    lwt () = Channel.write fd buf in
    cont ()

  let reads _ =
    failwith "TODO"

  let writes _ =
    failwith "TODO"

end

module Tag: IrminAPI.TAG
  with type local = local_tag
   and type remote = remote_tag
   and type channel = Channel.t
= struct

  module L = StringBase(struct
      type t = local_tag
      let to_string (L s) = s
      let of_string s = L s
    end)

  module R = StringBase(struct
      type t = remote_tag
      let to_string (R s) = s
      let of_string s = R s
    end)

  type local = L.t

  type remote = R.t

  type t =
    | Local of local
    | Remote of remote

  let remote (L s) = R s

  let local (R s) = L s

  type channel = Channel.t

  let to_string = function
    | Local t  -> Printf.sprintf "local/%s" (L.to_string t)
    | Remote t -> Printf.sprintf "remote/%s" (R.to_string t)

  let to_json = function
    | Local t  -> `O [ ("local" , L.to_json t) ]
    | Remote t -> `O [ ("remote", R.to_json t) ]

  let of_json = function
    | `O l ->
      let local = List.mem_assoc "local" l in
      let remote = List.mem_assoc "remote" l in
      if local && not remote then
        Local (L.of_json (List.assoc "local" l))
      else if remote && not local then
        Remote (R.of_json (List.assoc "remote" l))
      else if local && remote then
        failwith "TAG.of_json ('local' and 'remote')"
      else
        failwith "TAG.of_json (neither 'local' nor 'remote')"
    | _ -> failwith "TAG.of_json (not an object)"

  let read _ =
    failwith "TODO"

  let write _ =
    failwith "TODO"

  let reads _ =
    failwith "TODO"

  let writes _ =
    failwith "TODO"

end

(* Pair of tag * key *)
module TagKey (T: IrminAPI.BASE with type channel = Channel.t) = struct

  type t = T.t * Key.t

  let to_string (t, key) =
    Printf.sprintf "%s:%s" (T.to_string t) (Key.to_string key)

  let to_json (t, key) =
    `O [ ("tag", T.to_json t);
         ("key", Key.to_json key)]

  let of_json = function
    | `O l ->
      let tag =
        try List.assoc "tag" l
        with Not_found -> failwith "TagKey.of_json: missing tag" in
      let key =
        try List.assoc "key" l
        with Not_found -> failwith "TagKey.of_json: missing key" in
      (T.of_json tag, Key.of_json key)
    | _ -> failwith "TagKey.of_json: not an object"

  let read fd =
    lwt tag = T.read fd in
    lwt key = Key.read fd in
    return (tag, key)

  let write fd (tag, key) =
    lwt () = T.write fd tag in
    Key.write fd key

  include Iter(struct
      type t = T.t * Key.t
      let read = read
      let write = write
    end)

end


module Store = IrminMemory.Store(Key)(Value)
module Tag_store = IrminMemory.Tag_store(Tag)(Key)
