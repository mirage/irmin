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

  type tag = T of string

end

open Lwt
open Types

module Channel: API.CHANNEL
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

module StringBase: API.BASE
  with type t = string
   and type channel = Channel.t
= struct

  type t = string

  type channel = Channel.t

  let to_string s = s

  let to_json = JSON.of_string

  let of_json = JSON.to_string

  cstruct hdr {
      uint32_t length
    } as big_endian

  let read fd =
    lwt buf = Channel.read fd sizeof_hdr in
    let len = Int32.to_int (get_hdr_length buf) in
    lwt str = Channel.read_string fd len in
    return str

  let write fd str =
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
        lwt str = read fd in
        aux (str :: acc) (i-1) in
    aux [] len

  let writes fd strings =
    let len = List.length strings in
    let buf = Cstruct.create sizeof_hdr in
    set_hdr_length buf (Int32.of_int len);
    Lwt_list.iter_s (write fd) strings

end

module Blob: API.BASE
  with type t = blob
   and type channel = Channel.t
= struct

  type t = blob

  type channel = Channel.t

  let to_string (B b) =
    Printf.sprintf "%S" b

  let to_json (B b) =
    JSON.of_string b

  let of_json j =
    B (JSON.to_string j)

  let read fd =
    lwt str = StringBase.read fd in
    return (B str)

  let write fd (B string) =
    StringBase.write fd string

  let reads fd =
    lwt strings = StringBase.reads fd in
    return (List.map (fun b -> B b) strings)

  let writes fd blobs =
    let strings = List.map (fun (B b) -> b) blobs in
    StringBase.writes fd strings

end

module Key: API.KEY
  with type t = key
   and type channel = Channel.t
= struct

  type t = key

  type channel = Channel.t

  let to_string (K k) =
    Printf.sprintf "%s" k

  let to_json (K k) =
    JSON.of_string k

  let of_json j =
    K (JSON.to_string j)

  let create value =
    let str = Marshal.to_string value [] in
    K (Misc.sha1 str)

  let length (K k) =
    String.length k

  cstruct hdr {
      uint32_t key_length
    } as big_endian

  let read fd =
    lwt buf = Channel.read fd sizeof_hdr in
    let key_len = Int32.to_int (get_hdr_key_length buf) in
    lwt key = Channel.read_string fd key_len in
    return (K key)

  let write fd (K k) =
    let key_len = String.length k in
    let buf = Cstruct.create sizeof_hdr in
    set_hdr_key_length buf (Int32.of_int key_len);
    lwt () = Channel.write fd buf in
    Channel.write_string fd k

  let reads _ =
    failwith "TODO"

  let writes _ =
    failwith "TODO"

  module Vertex = struct
    type t = key
    let compare (K k1) (K k2) = String.compare k1 k2
    let hash (K k) = Hashtbl.hash k
    let equal (K k1) (K k2) = String.compare k1 k2 = 0
  end

  module Graph = struct

    type channel = Channel.t

    module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)

    include G

    let to_string _ =
      failwith "TODO"

    let to_json _ =
      failwith "TODO"

    let of_json _ =
      failwith "TODO"

    let read _ =
      failwith "TODO"

    let reads _ =
      failwith "TODO"

    let write _ =
      failwith "TODO"

    let writes _ =
      failwith "TODO"

  end

  type graph = Graph.t

end

module Revision: API.BASE
  with type t = revision
   and type channel = Channel.t = struct

  type t = revision

  type channel = Channel.t

  let to_string r =
    Printf.sprintf "[%s => %s]"
      (String.concat "-" (List.map Key.to_string r.parents))
      (Key.to_string r.contents)

  let of_json (json:JSON.t) = match json with
    | `O [ ("parents", parents); ("contents", contents) ] ->
      let parents = JSON.to_list Key.of_json parents in
      let parents = List.sort compare parents in
      let contents = Key.of_json contents in
      { parents; contents }
    | _ -> failwith "Revision.of_json"

  let to_json r =
    let parents = JSON.of_list Key.to_json r.parents in
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

module Value: API.BASE
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

module Tag: API.BASE
  with type t = tag
   and type channel = Channel.t
= struct

  type t = tag

  type channel = Channel.t

  let to_string (T t) =
    Printf.sprintf "%s" t

  let to_json (T t) =
    JSON.of_string t

  let of_json j =
    T (JSON.to_string j)

  let read fd =
    lwt string = StringBase.read fd in
    return (T string)

  let write fd (T string) =
    StringBase.write fd string

  let reads fd =
    lwt strings = StringBase.reads fd in
    return (List.map (fun t -> T t) strings)

  let writes fd tags =
    let strings = List.map (fun (T t) -> t) tags in
    StringBase.writes fd strings

end

module Store = Memory.Store(Key)(Value)
module Tag_store = Memory.Tag_store(Tag)(Key)
module Remote = Protocol.Make(Channel)(Key)(Tag)
