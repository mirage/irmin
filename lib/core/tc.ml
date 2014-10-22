(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Bin_prot.Std
open Sexplib.Std

exception Read_error

type 'a equal = 'a -> 'a -> bool
type 'a compare = 'a -> 'a -> int
type 'a hash = 'a -> int
type 'a to_sexp = 'a -> Sexplib.Sexp.t
type 'a reader = Mstruct.t -> 'a
type 'a size_of = 'a -> int
type 'a writer = 'a -> Cstruct.t -> Cstruct.t
type 'a to_json = 'a -> Ezjsonm.t
type 'a of_json = Ezjsonm.t -> 'a

module type I0 = sig
  type t
  val equal: t equal
  val compare: t compare
  val hash: t hash
  val to_sexp: t to_sexp
  val to_json: t to_json
  val of_json: t of_json
  val size_of: t size_of
  val write: t writer
  val read: t reader
end

module type I1 = sig
  type 'a t
  val equal: 'a equal -> 'a t equal
  val compare: 'a compare -> 'a t compare
  val hash: 'a hash -> 'a t hash
  val to_sexp: 'a to_sexp -> 'a t to_sexp
  val to_json: 'a to_json -> 'a t to_json
  val of_json: 'a of_json -> 'a t of_json
  val size_of: 'a size_of -> 'a t size_of
  val write: 'a writer -> 'a t writer
  val read: 'a reader -> 'a t reader
end

module type I2 = sig
  type ('a, 'b) t
  val equal: 'a equal -> 'b equal -> ('a, 'b) t equal
  val compare: 'a compare -> 'b compare -> ('a, 'b) t compare
  val hash: 'a hash -> 'b hash -> ('a, 'b) t hash
  val to_sexp: 'a to_sexp -> 'b to_sexp -> ('a, 'b) t to_sexp
  val to_json: 'a to_json -> 'b to_json -> ('a, 'b) t to_json
  val of_json: 'a of_json -> 'b of_json -> ('a, 'b) t of_json
  val size_of: 'a size_of -> 'b size_of -> ('a, 'b) t size_of
  val write: 'a writer -> 'b writer -> ('a, 'b) t writer
  val read: 'a reader -> 'b reader -> ('a, 'b) t reader
end

module type I3 = sig
  type ('a, 'b, 'c) t
  val equal: 'a equal -> 'b equal -> 'c equal -> ('a, 'b, 'c) t equal
  val compare: 'a compare -> 'b compare -> 'c compare -> ('a, 'b, 'c) t compare
  val hash: 'a hash -> 'b hash -> 'c hash -> ('a, 'b, 'c) t hash
  val to_sexp: 'a to_sexp -> 'b to_sexp -> 'c to_sexp -> ('a, 'b, 'c) t to_sexp
  val to_json: 'a to_json -> 'b to_json -> 'c to_json -> ('a, 'b, 'c) t to_json
  val of_json: 'a of_json -> 'b of_json -> 'c of_json -> ('a, 'b, 'c) t of_json
  val size_of: 'a size_of -> 'b size_of -> 'c size_of -> ('a, 'b, 'c) t size_of
  val write: 'a writer -> 'b writer -> 'c writer -> ('a, 'b, 'c) t writer
  val read: 'a reader -> 'b reader -> 'c reader -> ('a, 'b, 'c) t reader
end


let equal (type t) (module S: I0 with type t = t) = S.equal
let compare (type t) (module S: I0 with type t = t) = S.compare
let hash (type t) (module S: I0 with type t = t) = S.hash
let to_sexp (type t) (module S: I0 with type t = t) = S.to_sexp
let to_json (type t) (module S: I0 with type t = t) = S.to_json
let of_json (type t) (module S: I0 with type t = t) = S.of_json
let size_of (type t) (module S: I0 with type t = t) = S.size_of
let write (type t) (module S: I0 with type t = t) = S.write
let read (type t) (module S: I0 with type t = t) = S.read

let show (type t) (module S: I0 with type t = t) t =
  Sexplib.Sexp.to_string_hum (S.to_sexp t)

let shows (type t) (module S: I0 with type t = t) xs =
  List.map S.to_sexp xs
  |> fun l -> Sexplib.Sexp.to_string_hum (Sexplib.Sexp.List l)

let read_cstruct (type t) (module S: I0 with type t = t) buf =
  S.read (Mstruct.of_cstruct buf)

let write_cstruct (type t) (module S: I0 with type t = t) t =
  let buf = Cstruct.create (S.size_of t) in
  let buf = S.write t buf in
  buf

let read_string m str = read_cstruct m (Cstruct.of_string str)
let write_string m t = Cstruct.to_string (write_cstruct m t)

module Reader = struct

  let to_bin_prot (read_t:'a reader): 'a Bin_prot.Read.reader =
    fun buf ~pos_ref ->
      let off = !pos_ref in
      let b = Mstruct.of_bigarray ~off buf in
      let a = read_t b in
      pos_ref := Mstruct.offset b;
      a

  let of_bin_prot (bin_read_t: 'a Bin_prot.Read.reader): 'a reader =
    fun (buf: Mstruct.t) ->
      try
        let buffer = Mstruct.to_bigarray buf in
        let pos_ref = ref 0 in
        let t = bin_read_t buffer ~pos_ref in
        Mstruct.shift buf !pos_ref;
        t
      with Bin_prot.Common.Read_error _ ->
        raise Read_error

  let pair a b =
    of_bin_prot (Bin_prot.Read.bin_read_pair (to_bin_prot a) (to_bin_prot b))

  let list a =
    of_bin_prot (Bin_prot.Read.bin_read_list (to_bin_prot a))

  let map f = function
    | None  -> None
    | Some (b, t) -> Some (b, f t)

end

module Writer = struct

  let to_bin_prot (write:'a writer): 'a Bin_prot.Write.writer =
    fun buf ~pos t ->
      let b = Cstruct.of_bigarray ~off:pos buf in
      let b = write t b in
      b.Cstruct.off

  let of_bin_prot (bin_write_t: 'a Bin_prot.Write.writer): 'a writer =
    fun t ({ Cstruct.buffer; off; _ } as buf) ->
      let pos = bin_write_t buffer ~pos:off t in
      Cstruct.shift buf (pos - off)

  let pair a b =
    of_bin_prot (Bin_prot.Write.bin_write_pair (to_bin_prot a) (to_bin_prot b))

  let list a =
    of_bin_prot (Bin_prot.Write.bin_write_list (to_bin_prot a))

end

module Compare = struct

  let pair a b (k1, v1) (k2, v2) =
    match a k1 k2 with
    | 0 -> b v1 v2
    | x -> x

  let list a l1 l2 =
    let rec aux l1 l2 = match l1, l2 with
      | [], [] -> 0
      | [], _  -> -1
      | _ , [] -> 1
      | h1::t1, h2::t2 ->
        match a h1 h2 with
        | 0 -> aux t1 t2
        | x -> x
    in
    aux l1 l2

end

module Equal = struct

  let pair a b (k1, v1) (k2, v2) =
    a k1 k2 && b v1 v2

  let list a l1 l2 =
    let rec aux l1 l2 = match l1, l2 with
      | [], [] -> true
      | [], _  | _, [] -> false
      | h1::t1, h2::t2 -> a h1 h2 && aux l1 l2
    in
    aux l1 l2

end

module I0 (S: sig type t with sexp, bin_io, compare end) = struct

  include S
  let equal x y = compare x y = 0
  let hash = Hashtbl.hash
  let to_sexp = S.sexp_of_t
  let to_json t = Json.of_sexp (S.sexp_of_t t)
  let of_json t = S.t_of_sexp (Json.to_sexp t)

  open Bin_prot.Type_class

  let size_of = bin_size_t

  let read buf =
    try
      let pos_ref = ref 0 in
      let buffer = Mstruct.to_bigarray buf in
      let t = bin_t.reader.read ~pos_ref buffer in
      Mstruct.shift buf !pos_ref;
      t
    with Bin_prot.Common.Read_error _ ->
      raise Read_error

  let write t ({ Cstruct.buffer; off; _ } as buf) =
    let k = bin_t.writer.write buffer ~pos:off t in
    Cstruct.shift buf k

end

module App1(F: I1)(X: I0) = struct
  type t = X.t F.t
  let equal = F.equal X.equal
  let compare = F.compare X.compare
  let hash = F.hash X.hash
  let to_sexp = F.to_sexp X.to_sexp
  let to_json = F.to_json X.to_json
  let of_json = F.of_json X.of_json
  let size_of = F.size_of X.size_of
  let write = F.write X.write
  let read = F.read X.read
end

module I1 (S: sig type 'a t with sexp, compare, bin_io end):
  I1 with type 'a t = 'a S.t
= struct

  include S

  let equal equal_a x y =
    try S.compare (fun x y -> if equal_a x y then 0 else raise Exit) x y = 0
    with Exit -> false

  let to_sexp = S.sexp_of_t
  let hash _ = Hashtbl.hash

  let to_json json_of_a t =
    let open Sexplib.Type in
    let sexprs = ref [] in
    let sexp_of_a a =
      let marker = "__JSON__" ^ string_of_int (Random.int max_int) in
      sexprs := (marker, a) :: !sexprs;
      Atom marker in
    let rec json_of_sexp = function
      | List l -> Ezjsonm.list json_of_sexp l
      | Atom x ->
        try json_of_a (List.assq x !sexprs)
        with Not_found -> Json.encode_string x
    in
    json_of_sexp (S.sexp_of_t sexp_of_a t)

  let of_json a_of_json t =
    let open Sexplib.Type in
    let sexprs = ref [] in
    let rec sexp_of_json json =
      let e = match Json.decode_string json with
        | Some s -> Atom s
        | None   -> match json with
          | `A l -> List (List.map sexp_of_json l)
          | json  -> Atom (Ezjsonm.to_string json)
      in
      sexprs := (e, json) :: !sexprs;
      e
    in
    let a_of_sexp e = a_of_json (List.assq e !sexprs) in
    S.t_of_sexp a_of_sexp (sexp_of_json t)

  let size_of = bin_size_t

  let read read_a =
    let bin_read_a = Reader.to_bin_prot read_a in
    Reader.of_bin_prot (bin_read_t bin_read_a)

  let write write_a =
    let bin_write_a = Writer.to_bin_prot write_a in
    Writer.of_bin_prot (bin_write_t bin_write_a)

end

module App2(F: I2)(X: I0)(Y: I0) = struct
  type t = (X.t, Y.t) F.t
  let equal = F.equal X.equal Y.equal
  let compare = F.compare X.compare Y.compare
  let hash = F.hash X.hash Y.hash
  let to_sexp = F.to_sexp X.to_sexp Y.to_sexp
  let to_json = F.to_json X.to_json Y.to_json
  let of_json = F.of_json X.of_json Y.of_json
  let size_of = F.size_of X.size_of Y.size_of
  let write = F.write X.write Y.write
  let read = F.read X.read Y.read
end

module I2 (S: sig type ('a, 'b) t with sexp, compare, bin_io end):
  I2 with type ('a, 'b) t = ('a, 'b) S.t
= struct

  include S

  let equal equal_a equal_b x y =
    let compare_a x y = if equal_a x y then 0 else raise Exit in
    let compare_b x y = if equal_b x y then 0 else raise Exit in
    try S.compare compare_a compare_b x y = 0
    with Exit -> false

  let hash _ _ = Hashtbl.hash
  let to_sexp = S.sexp_of_t
  let size_of = S.bin_size_t

  let read read_a read_b =
    let bin_read_a = Reader.to_bin_prot read_a in
    let bin_read_b = Reader.to_bin_prot read_b in
    Reader.of_bin_prot (S.bin_read_t bin_read_a bin_read_b)

  let write write_a write_b =
    let bin_write_a = Writer.to_bin_prot write_a in
    let bin_write_b = Writer.to_bin_prot write_b in
    Writer.of_bin_prot (S.bin_write_t bin_write_a bin_write_b)

  let to_json json_of_a json_of_b t =
    let open Sexplib.Type in
    let sexprs_a = ref [] in
    let sexp_of_a a =
      let marker = "__JSON__A_" ^ string_of_int (Random.int max_int) in
      sexprs_a := (marker, a) :: !sexprs_a;
      Atom marker in
    let sexprs_b = ref [] in
    let sexp_of_b b =
      let marker = "__JSON__B_" ^ string_of_int (Random.int max_int) in
      sexprs_b := (marker, b) :: !sexprs_b;
      Atom marker in
    let rec json_of_sexp = function
      | List l -> Ezjsonm.list json_of_sexp l
      | Atom x ->
        try json_of_a (List.assq x !sexprs_a)
        with Not_found ->
          try json_of_b (List.assq x !sexprs_b)
          with Not_found -> Json.encode_string x
    in
    json_of_sexp (S.sexp_of_t sexp_of_a sexp_of_b t)

  let of_json a_of_json b_of_json t =
    let open Sexplib.Type in
    let sexprs = ref [] in
    let rec sexp_of_json json =
      let e = match Json.decode_string json with
        | Some s -> Atom s
        | None   -> match json with
          | `A l -> List (List.map sexp_of_json l)
          | json  -> Atom (Ezjsonm.to_string json)
      in
      sexprs := (e, json) :: !sexprs;
      e
    in
    let a_of_sexp e = a_of_json (List.assq e !sexprs) in
    let b_of_sexp e = b_of_json (List.assq e !sexprs) in
    S.t_of_sexp a_of_sexp b_of_sexp (sexp_of_json t)

end

module App3(F: I3)(X: I0)(Y: I0)(Z: I0) = struct
  type t = (X.t, Y.t, Z.t) F.t
  let equal = F.equal X.equal Y.equal Z.equal
  let compare = F.compare X.compare Y.compare Z.compare
  let hash = F.hash X.hash Y.hash Z.hash
  let to_sexp = F.to_sexp X.to_sexp Y.to_sexp Z.to_sexp
  let to_json = F.to_json X.to_json Y.to_json Z.to_json
  let of_json = F.of_json X.of_json Y.of_json Z.of_json
  let size_of = F.size_of X.size_of Y.size_of Z.size_of
  let write = F.write X.write Y.write Z.write
  let read = F.read X.read Y.read Z.read
end

module I3 (S: sig type ('a, 'b, 'c) t with sexp, compare, bin_io end):
  I3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) S.t
= struct

  include S

  let equal equal_a equal_b equal_c x y =
    let compare_a x y = if equal_a x y then 0 else raise Exit in
    let compare_b x y = if equal_b x y then 0 else raise Exit in
    let compare_c x y = if equal_c x y then 0 else raise Exit in
    try S.compare compare_a compare_b compare_c x y = 0
    with Exit -> false

  let hash _ _ _ = Hashtbl.hash
  let to_sexp = S.sexp_of_t
  let size_of = S.bin_size_t

  let read read_a read_b read_c =
    let bin_read_a = Reader.to_bin_prot read_a in
    let bin_read_b = Reader.to_bin_prot read_b in
    let bin_read_c = Reader.to_bin_prot read_c in
    Reader.of_bin_prot (S.bin_read_t bin_read_a bin_read_b bin_read_c)

  let write write_a write_b write_c =
    let bin_write_a = Writer.to_bin_prot write_a in
    let bin_write_b = Writer.to_bin_prot write_b in
    let bin_write_c = Writer.to_bin_prot write_c in
    Writer.of_bin_prot (S.bin_write_t bin_write_a bin_write_b bin_write_c)

  let to_json json_of_a json_of_b json_of_c t =
    let open Sexplib.Type in
    let sexprs_a = ref [] in
    let sexp_of_a a =
      let marker = "__JSON__A_" ^ string_of_int (Random.int max_int) in
      sexprs_a := (marker, a) :: !sexprs_a;
      Atom marker in
    let sexprs_b = ref [] in
    let sexp_of_b b =
      let marker = "__JSON__B_" ^ string_of_int (Random.int max_int) in
      sexprs_b := (marker, b) :: !sexprs_b;
      Atom marker in
    let sexprs_c = ref [] in
    let sexp_of_c c =
      let marker = "__JSON__C_" ^ string_of_int (Random.int max_int) in
      sexprs_c := (marker, c) :: !sexprs_c;
      Atom marker in
    let rec json_of_sexp = function
      | List l -> Ezjsonm.list json_of_sexp l
      | Atom x ->
        try json_of_a (List.assq x !sexprs_a)
        with Not_found ->
          try json_of_b (List.assq x !sexprs_b)
          with Not_found ->
            try json_of_c (List.assq x !sexprs_c)
            with Not_found -> Json.encode_string x
    in
    json_of_sexp (S.sexp_of_t sexp_of_a sexp_of_b sexp_of_c t)

  let of_json a_of_json b_of_json c_of_json t =
    let open Sexplib.Type in
    let sexprs = ref [] in
    let rec sexp_of_json json =
      let e = match Json.decode_string json with
        | Some s -> Atom s
        | None   -> match json with
          | `A l -> List (List.map sexp_of_json l)
          | json  -> Atom (Ezjsonm.to_string json)
      in
      sexprs := (e, json) :: !sexprs;
      e
    in
    let a_of_sexp e = a_of_json (List.assq e !sexprs) in
    let b_of_sexp e = b_of_json (List.assq e !sexprs) in
    let c_of_sexp e = c_of_json (List.assq e !sexprs) in
    S.t_of_sexp a_of_sexp b_of_sexp c_of_sexp (sexp_of_json t)

end

module L0 (S: sig
            type t
            module K: I0
            val to_list: t -> K.t list
            val of_list: K.t list -> t
          end) =
struct

  let compare t1 t2 =
    let rec aux t1 t2 = match t1, t2 with
      | [], [] -> 0
      | _ , [] -> 1
      | [], _  -> -1
      | h1::t1, h2::t2 -> match S.K.compare h1 h2 with
        | 0 -> aux t1 t2
        | i -> i
    in
    aux (S.to_list t1) (S.to_list t2)

  let equal t1 t2 = List.for_all2 S.K.equal (S.to_list t1) (S.to_list t2)
  let hash = Hashtbl.hash
  let to_sexp t = Sexplib.Conv.sexp_of_list S.K.to_sexp (S.to_list t)
  let to_json t = Ezjsonm.list S.K.to_json (S.to_list t)
  let of_json j = S.of_list (Ezjsonm.get_list S.K.of_json j)
  let size_of t = Bin_prot.Size.bin_size_list S.K.size_of (S.to_list t)

  let write t =
    Writer.list S.K.write (S.to_list t)

  let read buf =
    let x = Reader.list S.K.read buf in
    S.of_list x

end

module L1 (S: sig
            type 'a t
            val to_list: 'a t -> 'a list
            val of_list: 'a list -> 'a t
          end) =
struct

  let compare compare_a t1 t2 =
    let rec aux t1 t2 = match t1, t2 with
      | [], [] -> 0
      | _ , [] -> 1
      | [], _  -> -1
      | h1::t1, h2::t2 -> match compare_a h1 h2 with
        | 0 -> aux t1 t2
        | i -> i
    in
    aux (S.to_list t1) (S.to_list t2)

  let equal equal_a t1 t2 = List.for_all2 equal_a (S.to_list t1) (S.to_list t2)
  let hash _ = Hashtbl.hash
  let to_sexp to_sexp_a t = Sexplib.Conv.sexp_of_list to_sexp_a (S.to_list t)
  let to_json to_json_a t = Ezjsonm.list to_json_a (S.to_list t)
  let of_json of_json_a j = S.of_list (Ezjsonm.get_list of_json_a j)
  let size_of size_of_a t = Bin_prot.Size.bin_size_list size_of_a (S.to_list t)

  let write write_a t =
    Writer.list write_a (S.to_list t)

  let read read_a buf =
    let x = Reader.list read_a buf in
    S.of_list x

end

module AL (L: sig
    type 'a t
    module K: I0
    val to_alist: 'a t -> (K.t * 'a) list
    val of_alist: (K.t * 'a) list -> 'a t
  end) =
struct

  let hash _ = Hashtbl.hash

  let to_sexp sexp_of_a t =
    let l = L.to_alist t in
    Sexplib.Conv.sexp_of_list (Sexplib.Conv.sexp_of_pair L.K.to_sexp sexp_of_a) l

  let to_json json_of_a t =
    let l = L.to_alist t in
    Ezjsonm.(list (pair L.K.to_json json_of_a) l)

  let of_json a_of_json json =
    let l = Ezjsonm.(get_list (get_pair L.K.of_json a_of_json) json) in
    L.of_alist l

  let size_of size_of_a t =
    let size_of_pair = Bin_prot.Size.bin_size_pair L.K.size_of size_of_a in
    Bin_prot.Size.bin_size_list size_of_pair (L.to_alist t)

  let read read_a buf =
    let l = Reader.list (Reader.pair L.K.read read_a) buf in
    L.of_alist l

  let write write_a t =
    let bin_write_k = Writer.to_bin_prot L.K.write in
    let bin_write_a = Writer.to_bin_prot write_a in
    let bindings =
      let bin = Bin_prot.Write.bin_write_pair bin_write_k bin_write_a in
      Writer.of_bin_prot bin
    in
    Writer.list bindings (L.to_alist t)

  let compare_bindings compare_a (k1, v1) (k2, v2) =
    match L.K.compare k1 k2 with
    | 0 -> compare_a v1 v2
    | x -> x

  let compare compare_a m1 m2 =
    let compare = compare_bindings compare_a in
    let l1 = List.sort compare (L.to_alist m1) in
    let l2 = List.sort compare (L.to_alist m2) in
    let rec aux t1 t2 = match t1, t2 with
      | [], [] -> 0
      | [], _  -> -1
      | _ , [] -> 1
      | h1::t1, h2::t2 ->
        match compare h1 h2 with
        | 0 -> aux t1 t2
        | x -> x
    in
    aux l1 l2

  let equal equal_a t1 t2 =
    let compare = compare_bindings (fun _ _ -> 0) in
    let l1 = List.sort compare (L.to_alist t1) in
    let l2 = List.sort compare (L.to_alist t2) in
    let f (k1, v1) (k2, v2) = L.K.equal k1 k2 && equal_a v1 v2 in
    List.for_all2 f l1 l2

end

module S = I0(struct type t = string with compare, sexp, bin_io end)
module U = I0(struct type t = unit with compare, sexp, bin_io end)
module O = I1(struct type 'a t = 'a option with compare, sexp, bin_io end)
module P = I2(struct type ('a, 'b) t = 'a * 'b with compare, sexp, bin_io end)
module I = I0(struct type t = int with compare, sexp, bin_io end)
module L = I1(struct type 'a t = 'a list with sexp, compare, bin_io end)
