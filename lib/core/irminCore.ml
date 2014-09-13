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

open Printf
open Bin_prot.Std
open Sexplib.Std

type 'a equal = 'a -> 'a -> bool
type 'a compare = 'a -> 'a -> int
type 'a hash = 'a -> int
type 'a to_sexp = 'a -> Sexplib.Sexp.t
type 'a to_json = 'a -> Ezjsonm.t
type 'a of_json = Ezjsonm.t -> 'a
type 'a size_of = 'a -> int
type 'a writer = 'a -> Cstruct.t -> Cstruct.t
type 'a reader = Cstruct.t -> (Cstruct.t * 'a) option

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

let equal (type t) (module S: I0 with type t = t) = S.equal
let compare (type t) (module S: I0 with type t = t) = S.compare
let hash (type t) (module S: I0 with type t = t) = S.hash
let to_sexp (type t) (module S: I0 with type t = t) = S.to_sexp
let to_json (type t) (module S: I0 with type t = t) = S.to_json
let of_json (type t) (module S: I0 with type t = t) = S.of_json
let size_of (type t) (module S: I0 with type t = t) = S.size_of
let write (type t) (module S: I0 with type t = t) = S.write
let read (type t) (module S: I0 with type t = t) = S.read

let force oc s = output_string oc (Lazy.force s)

let pretty (type t) (module S: I0 with type t = t) t =
  lazy (
    Sexplib.Sexp.to_string_hum (S.to_sexp t)
  )

let prettys (type t) (module S: I0 with type t = t) xs =
  lazy (
    List.map S.to_sexp xs
    |> fun l -> Sexplib.Sexp.to_string_hum (Sexplib.Sexp.List l)
  )

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

let read_all read ba =
  let buf = Cstruct.of_bigarray ba in
  match read buf with
  | None -> None
  | Some (_, t) ->
    (* FIXME: assert the buffer is empty? *)
    Some t

let write_all (size_of:'a size_of) (write:'a writer) (t:'a) =
  let buf = Cstruct.create (size_of t) in
  let buf = write t buf in
  (* FIXME: assert len=off *)
  buf.Cstruct.buffer

module Reader = struct

  let to_bin_prot read_t =
    let raise_err pos =
      Bin_prot.Common.(raise_read_error (ReadError.Silly_type "?") pos)
    in
    fun buf ~pos_ref ->
      let off = !pos_ref in
      let b = Cstruct.of_bigarray ~off buf in
      match read_t b with
      | None -> raise_err off
      | Some (b, a) ->
        pos_ref := b.Cstruct.off;
        a

  let of_bin_prot bin_read_t =
    fun ({ Cstruct.buffer; off; _ } as buf) ->
      try
        let pos_ref = ref off in
        let t = bin_read_t buffer ~pos_ref in
        let buf = Cstruct.shift buf (!pos_ref - off) in
        Some (buf, t)
      with Bin_prot.Common.Read_error _ ->
        None

  let pair a b =
    of_bin_prot (Bin_prot.Read.bin_read_pair (to_bin_prot a) (to_bin_prot b))

  let list a =
    of_bin_prot (Bin_prot.Read.bin_read_list (to_bin_prot a))

  let map f = function
    | None  -> None
    | Some (b, t) -> Some (b, f t)

end

module Writer = struct

  let to_bin_prot write =
    fun buf ~pos t ->
      let b = Cstruct.of_bigarray ~off:pos buf in
      let b = write t b in
      b.Cstruct.off

  let of_bin_prot bin_write_t =
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

module Hex = struct

  (* From OCaml's stdlib. See [Digest.to_hex] *)
  let encode s =
    let n = String.length s in
    let result = String.create (n*2) in
    for i = 0 to n-1 do
      String.blit (Printf.sprintf "%02x" (int_of_char s.[i])) 0 result (2*i) 2;
    done;
    result

  (* From OCaml's stdlib. See [Digest.from_hex] *)
  let decode h =
    let n = String.length h in
    if n mod 2 <> 0 then (
      let msg =
        Printf.sprintf "hex_decode: wrong string size for %S (%d)" h (String.length h) in
      raise (Invalid_argument msg)
    );
    let digit c =
      match c with
      | '0'..'9' -> Char.code c - Char.code '0'
      | 'A'..'F' -> Char.code c - Char.code 'A' + 10
      | 'a'..'f' -> Char.code c - Char.code 'a' + 10
      | c ->
        let msg = Printf.sprintf "hex_decode: %S is invalid" (String.make 1 c) in
        raise (Invalid_argument msg) in
    let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
    let result = String.create (n / 2) in
    for i = 0 to n/2 - 1 do
      result.[i] <- Char.chr (byte (2 * i));
    done;
    result

end

module JSON = struct

  let is_valid_utf8 str =
    try
      Uutf.String.fold_utf_8 (fun _ _ -> function
          | `Malformed _ -> raise (Failure "utf8")
          | _ -> ()
        ) () str;
      true
    with Failure "utf8" -> false

  let encode_string str =
    if is_valid_utf8 str
    then Ezjsonm.string str
    else `O [ "hex", Ezjsonm.string (Hex.encode str) ]

  let decode_string = function
    | `String str               -> Some str
    | `O [ "hex", `String str ] -> Some (Hex.decode str)
    | j                         -> None

  let decode_string_exn j =
    match decode_string j with
    | Some s -> s
    | None   ->
      failwith (
        Printf.sprintf "%s is not a valid UT8-encoded JSON string"
          (Ezjsonm.to_string j)
      )

  let rec of_sexp = function
    | Sexplib.Type.Atom x -> encode_string x
    | Sexplib.Type.List l -> Ezjsonm.list of_sexp l

  let rec to_sexp json =
    match decode_string json with
    | Some s -> Sexplib.Type.Atom s
    | None   ->
      match json with
      | `A l -> Sexplib.Type.List (List.map to_sexp l)
      | _    -> failwith (sprintf "sexp_of_json: %s" (Ezjsonm.to_string json))

end

let invalid_argf fmt =
  ksprintf (fun str ->
      Invalid_argument str
    ) fmt

module I0 (S: sig type t with sexp, bin_io, compare end) = struct

  include S
  let equal x y = compare x y = 0
  let hash = Hashtbl.hash
  let to_sexp = S.sexp_of_t
  let to_json t = JSON.of_sexp (S.sexp_of_t t)
  let of_json t = S.t_of_sexp (JSON.to_sexp t)

  open Bin_prot.Type_class

  let size_of = bin_size_t

  let read ({ Cstruct.buffer; off; _ } as buf) =
    try
      let pos_ref = ref off in
      let t = bin_t.reader.read ~pos_ref buffer in
      let buf = Cstruct.shift buf (!pos_ref - off) in
      Some (buf, t)
    with Bin_prot.Common.Read_error _ ->
      None

  let write t ({ Cstruct.buffer; off; _ } as buf) =
    let k = bin_t.writer.write buffer ~pos:off t in
    Cstruct.shift buf k

end

module Char = struct
  include I0(struct type t = char with sexp, compare, bin_io end)
  let to_int = Char.code
  let of_int i = if i >= 0 && i <= 255 then Some (Char.chr i) else None
  let of_int_exn i = match of_int i with
    | None   -> raise (invalid_argf "Char.of_int_exn: %d is out of range." i)
    | Some c -> c
end

module String = struct
  include I0(struct type t = string with sexp, compare, bin_io end)
  let of_json = JSON.decode_string_exn
  let to_json = JSON.encode_string
  let create = String.create
  let make = String.make
  let get = String.get
  let set = String.set
  let blit = String.blit
  let is_empty t = t = ""
  let sub str ~pos ~len = String.sub str pos len
  let length = String.length
  let concat ts ~sep = String.concat sep ts
  let escaped = String.escaped

  let split str ~on =
    let len = String.length str in
    let rec loop acc i =
      if i < 0 then acc else (
        Printf.printf "i=%d\n%!" i;
        let j =
          try String.rindex_from str i on
          with Not_found -> -42
        in
        match j with
        | -42 -> String.sub str 0 i :: acc
        | _  ->
          let sub = String.sub str (j + 1) (i - j) in
          loop (sub :: acc) (j - 1)
      )
    in
    loop [] (len - 1)

  let replace ~pattern subst str =
    let rex = Re_perl.compile_pat pattern in
    Re_pcre.substitute ~rex ~subst str

  module Hex = Hex

end

module Int = struct
  include I0(struct type t = int with sexp, compare, bin_io end)
  let of_json = Ezjsonm.get_int
  let to_json = Ezjsonm.int
  let max_value = max_int
end

module Bigstring = struct
  open Bigarray
  module M = struct
    include Bin_prot.Std
    include Sexplib.Conv
    type t = bigstring with bin_io
    let sexp_of_t = Sexplib.Conv.sexp_of_bigstring
    let t_of_sexp = Sexplib.Conv.bigstring_of_sexp
    let compare = Pervasives.compare (* FIXME *)
  end
  include I0(M)
  let create len = Array1.create char c_layout len
  let length t = Array1.dim t

  external unsafe_blit_bigstring_to_string:
    t -> int -> string -> int -> int -> unit = "caml_blit_bigstring_to_string"
      "noalloc"

  external unsafe_blit_string_to_bigstring:
    string -> int -> t -> int -> int -> unit = "caml_blit_string_to_bigstring"
      "noalloc"

  let to_string t =
    let len = length t in
    let str = String.create len in
    unsafe_blit_bigstring_to_string t 0 str 0 len;
    str

  let of_string str =
    let len = String.length str in
    let t = create len in
    unsafe_blit_string_to_bigstring str 0 t 0 len;
    t

end

module Unit = I0(struct type t = unit with sexp, bin_io, compare end)

module Int64 = struct
  include I0(struct type t = int64 with sexp, bin_io, compare end)
  let (+) = Int64.add
  let to_string = Int64.to_string
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
        with Not_found -> String.to_json x
    in
    json_of_sexp (S.sexp_of_t sexp_of_a t)

  let of_json a_of_json t =
    let open Sexplib.Type in
    let sexprs = ref [] in
    let rec sexp_of_json json =
      let e = match JSON.decode_string json with
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
          with Not_found -> String.to_json x
    in
    json_of_sexp (S.sexp_of_t sexp_of_a sexp_of_b t)

  let of_json a_of_json b_of_json t =
    let open Sexplib.Type in
    let sexprs = ref [] in
    let rec sexp_of_json json =
      let e = match JSON.decode_string json with
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
            with Not_found -> String.to_json x
    in
    json_of_sexp (S.sexp_of_t sexp_of_a sexp_of_b sexp_of_c t)

  let of_json a_of_json b_of_json c_of_json t =
    let open Sexplib.Type in
    let sexprs = ref [] in
    let rec sexp_of_json json =
      let e = match JSON.decode_string json with
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

module Out_channel = struct
  type t = out_channel
  let create = open_out
  let close = close_out
end

module Option = struct
  include I1(struct type 'a t = 'a option with sexp, bin_io, compare end)
end

module Pair = struct
  include I2(struct type ('a, 'b) t = 'a * 'b with sexp, bin_io, compare end)
end

module List = struct
  include I1(struct type 'a t = 'a list with sexp, compare, bin_io end)
  let length = List.length
  let iter t ~f = List.iter f t
  let rev = List.rev
  let sort t ~cmp = List.sort cmp t
  let for_all2 t ~f = List.for_all2 f t
  let fold_left t ~init ~f = List.fold_left f init t
  let mem t x = List.mem x t
  let map t ~f = List.map f t
  let rev_map t ~f = List.rev_map f t
  let filter t ~f = List.filter f t
  let exists t ~f = List.exists f t
  let sort ?(compare=Pervasives.compare) t = List.sort compare t
  let partition_map t ~f =
    let rec aux fst snd = function
      | []   -> List.rev fst, List.rev snd
      | h::t ->
        match f h with
        | `Fst x -> aux (x :: fst) snd t
        | `Snd x -> aux fst (x :: snd) t
    in
    aux [] [] t

  let filter_map t ~f =
    let rec aux acc = function
      | []   -> List.rev acc
      | h::t ->
        match f h with
        | None   -> aux acc t
        | Some x -> aux (x::acc) t
    in
    aux [] t

  let dedup ?(compare=Pervasives.compare) t =
    let t = List.sort compare t in
    let rec aux acc = function
      | []      -> List.rev acc
      | [x]     -> aux (x :: acc) []
      | x::(y::t as tl) ->
        match compare x y with
        | 0 -> aux acc tl
        | _ -> aux (x :: acc) tl
    in
    aux [] t

  module Assoc = struct

    include I2(struct
        type ('a, 'b) t = ('a * 'b) list with sexp, compare, bin_io
      end)

    let find_exn t ?(equal=(=)) a =
      let fn (k, _) = equal k a in
      snd (List.find fn t)

    let find t ?equal a =
      try Some (find_exn t ?equal a)
      with Not_found -> None

  end

  let pretty f = function
    | [] -> "{}"
    | l  ->
      let buf = Buffer.create 1024 in
      let len = ref (List.length l - 1) in
      Buffer.add_string buf "{ ";
      iter ~f:(fun e ->
          Buffer.add_string buf (f e);
          if !len > 0 then Buffer.add_string buf ", ";
          decr len
        ) l;
      Buffer.add_string buf " }";
      Buffer.contents buf

end

module type ListLike0 = sig
  type t
  module K: I0
  val to_list: t -> K.t list
  val of_list: K.t list -> t
end

module ListLike0 (S: ListLike0) = struct
  include S
  let equal t1 t2 = List.equal S.K.equal (S.to_list t1) (S.to_list t2)
  let compare t1 t2 = List.compare S.K.compare (S.to_list t1) (S.to_list t2)
  let hash = Hashtbl.hash
  let to_sexp t = List.to_sexp S.K.to_sexp (S.to_list t)
  let to_json t = List.to_json S.K.to_json (S.to_list t)
  let of_json j = S.of_list (List.of_json S.K.of_json j)
  let size_of t = List.size_of S.K.size_of (S.to_list t)
  let write t = List.write S.K.write (S.to_list t)
  let read buf = Reader.map S.of_list (List.read S.K.read buf)
end

module type ListLike1 = sig
  type 'a t
  val to_list: 'a t -> 'a list
  val of_list: 'a list -> 'a t
end

module ListLike1 (S: ListLike1) = struct
  include S
  let equal equal_a t1 t2 = List.equal equal_a (S.to_list t1) (S.to_list t2)
  let compare compare_a t1 t2 = List.compare compare_a (S.to_list t1) (S.to_list t2)
  let hash _ = Hashtbl.hash
  let to_sexp to_sexp_a t = List.to_sexp to_sexp_a (S.to_list t)
  let to_json to_json_a t = List.to_json to_json_a (S.to_list t)
  let of_json of_json_a j = S.of_list (List.of_json of_json_a j)
  let size_of size_of_a t = List.size_of size_of_a (S.to_list t)
  let write write_a t = List.write write_a (S.to_list t)
  let read read_a buf = Reader.map S.of_list (List.read read_a buf)
end

module type AListLike1 = sig
  type 'a t
  module K: I0
  val to_alist: 'a t -> (K.t * 'a) list
  val of_alist: (K.t * 'a) list -> [`Ok of 'a t | `Duplicate_key of K.t]
  val of_alist_exn: (K.t * 'a) list -> 'a t
end

module AListLike1 (L: AListLike1) = struct

  include L

  let hash _ = Hashtbl.hash

  let to_sexp sexp_of_a t =
    let l = L.to_alist t in
    List.to_sexp (Sexplib.Conv.sexp_of_pair L.K.to_sexp sexp_of_a) l

  let to_json json_of_a t =
    let l = L.to_alist t in
    Ezjsonm.(list (pair L.K.to_json json_of_a) l)

  let of_json a_of_json json =
    let l = Ezjsonm.(get_list (get_pair L.K.of_json a_of_json) json) in
    L.of_alist_exn l

  let size_of size_of_a t =
    let size_of_pair = Bin_prot.Size.bin_size_pair L.K.size_of size_of_a in
    List.size_of size_of_pair (L.to_alist t)

  let read read_a buf =
    match List.read (Reader.pair L.K.read read_a) buf with
    | None          -> None
    | Some (buf, l) ->
      match L.of_alist l with
      | `Ok l -> Some (buf, l)
      | `Duplicate_key _ -> None

  let write write_a t =
    let bin_write_k = Writer.to_bin_prot L.K.write in
    let bin_write_a = Writer.to_bin_prot write_a in
    let bindings =
      let bin = Bin_prot.Write.bin_write_pair bin_write_k bin_write_a in
      Writer.of_bin_prot bin
    in
    List.write bindings (L.to_alist t)

  let compare_bindings compare_a (k1, v1) (k2, v2) =
    match L.K.compare k1 k2 with
    | 0 -> compare_a v1 v2
    | x -> x

  let compare compare_a m1 m2 =
    let compare = compare_bindings compare_a in
    let l1 = List.sort ~compare (L.to_alist m1) in
    let l2 = List.sort ~compare (L.to_alist m2) in
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
    let l1 = List.sort ~compare (L.to_alist t1) in
    let l2 = List.sort ~compare (L.to_alist t2) in
    let f (k1, v1) (k2, v2) = L.K.equal k1 k2 && equal_a v1 v2 in
    List.for_all2 ~f l1 l2

end


module Queue = struct
  let create = Queue.create
  let enqueue t x = Queue.push x t
  let dequeue_exn t = Queue.pop t
  let dequeue t =
    try Some (dequeue_exn t)
    with Queue.Empty -> None
  module L = struct
    type 'a t = 'a Queue.t
    let to_list t =
      let l = ref [] in
      Queue.iter (fun x -> l := x :: !l) t;
      !l
    let of_list l =
      let t = Queue.create () in
      List.iter ~f:(enqueue t) l;
      t
  end
  include ListLike1(L)
end

module Stack = struct
  let create = Stack.create
  let push t x = Stack.push x t
  let pop_exn = Stack.pop
  let pop t =
    try Some (pop_exn t)
    with Stack.Empty -> None
  module L = struct
    type 'a t = 'a Stack.t
    let to_list t =
      let l = ref [] in
      Stack.iter (fun e -> l := e :: !l) t;
      List.rev !l
    let of_list l =
      let t = Stack.create () in
      List.iter ~f:(push t) (List.rev l);
      t
  end
  include ListLike1(L)
end

module Set = struct
  module type S = sig
    include I0
    type elt
    val empty: t
    val singleton: elt -> t
    val is_empty: t -> bool
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val of_list: elt list -> t
    val to_list: t -> elt list
  end
  module Make (K: I0) = struct
    module Set = Set.Make(K)
    module L = struct
      module K = K
      type t = Set.t
      let to_list t =
        let l = ref [] in
        Set.iter (fun k -> l := k :: !l) t;
        List.rev !l
      let of_list l =
        let t = ref Set.empty in
        List.iter ~f:(fun k -> t := Set.add k !t) l;
        !t
    end
    include ListLike0(L)
    type elt = K.t
    let empty = Set.empty
    let singleton = Set.singleton
    let is_empty = Set.is_empty
    let union = Set.union
    let inter = Set.inter
    let diff = Set.diff
  end
end

module type Dictionary = sig
  include I1
  type key
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> [`Ok of 'a t | `Duplicate_key of key]
  val of_alist_exn: (key * 'a) list -> 'a t
  val keys: 'a t -> key list
  val is_empty: 'a t -> bool
  val mem: 'a t -> key -> bool
  val find: 'a t -> key -> 'a option
  val fold: 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b
  val map: 'a t -> f:('a -> 'b) -> 'b t
  val iter: 'a t -> f:(key:key -> data:'a -> unit) -> unit
  val filter: 'a t -> f:(key:key -> data:'a -> bool) -> 'a t
end

module Hashtbl = struct
  module type S = sig
    include Dictionary
    val create: ?size:int -> unit -> 'a t
    val clear: 'a t -> unit
    val of_alist_add: (key * 'a) list -> 'a t
    val replace: 'a t -> key:key -> data:'a -> unit
    val add: 'a t -> key:key -> data:'a -> [`Ok | `Duplicate]
    val add_exn: 'a t -> key:key -> data:'a -> unit
    val add_multi: 'a list t -> key:key -> data:'a -> unit
    val remove: 'a t -> key -> unit
  end

  let hash = Hashtbl.hash

  module Make (K: I0) = struct

    type 'a t = (K.t, 'a) Hashtbl.t
    type key = K.t
    let hash _ = Hashtbl.hash
    let to_sexp sexp_of_a = Sexplib.Conv.sexp_of_hashtbl K.to_sexp sexp_of_a

    let of_alist_add l =
      let acc = Hashtbl.create (List.length l) in
      List.iter ~f:(fun (k, v) ->
          Hashtbl.add acc k v
        ) l;
      acc

    exception D of K.t

    let of_alist l =
      let acc = Hashtbl.create (List.length l) in
      try
        List.iter ~f:(fun (k, v) ->
            if Hashtbl.mem acc k then raise (D k)
            else Hashtbl.add acc k v
          ) l;
        `Ok acc
      with D k ->
        `Duplicate_key k

    let of_alist_exn l =
      match of_alist l with
      | `Ok acc -> acc
      | `Duplicate_key k ->
        raise (invalid_argf "Duplicate key: %s"
                 (Lazy.force (pretty (module K) k)))

    let to_alist t =
      let acc = ref [] in
      Hashtbl.iter (fun k v ->
          acc := (k, v) :: !acc
        ) t;
      List.rev !acc

    let of_json a_of_json json =
      let dict = Ezjsonm.get_list (Ezjsonm.get_pair K.of_json a_of_json) json in
      of_alist_add dict

    let to_json json_of_a t =
      let dict = to_alist t in
      Ezjsonm.list (Ezjsonm.pair K.to_json json_of_a) dict

    let size_of size_of_a = Bin_prot.Size.bin_size_hashtbl K.size_of size_of_a

    let read read_a =
      let bin_read_k = Reader.to_bin_prot K.read in
      let bin_read_a = Reader.to_bin_prot read_a in
      let bin_t = Bin_prot.Read.bin_read_hashtbl bin_read_k bin_read_a in
      Reader.of_bin_prot bin_t

    let write write_a =
      let bin_write_k = Writer.to_bin_prot K.write in
      let bin_write_a = Writer.to_bin_prot write_a in
      let bin_t = Bin_prot.Write.bin_write_hashtbl bin_write_k bin_write_a in
      Writer.of_bin_prot bin_t

    let compare compare_a t1 t2 =
      let compare = Compare.pair K.compare compare_a in
      let l1 = List.sort ~compare (to_alist t1) in
      let l2 = List.sort ~compare (to_alist t2) in
      Compare.list compare l1 l2

    let equal equal_a t1 t2 =
      let compare = Compare.pair K.compare (fun _ _ -> 0) in
      let l1 = List.sort ~compare (to_alist t1) in
      let l2 = List.sort ~compare (to_alist t2) in
      Equal.list (Equal.pair K.equal equal_a) l1 l2

    let remove = Hashtbl.remove
    let mem = Hashtbl.mem
    let clear = Hashtbl.clear
    let create ?(size=128) () = Hashtbl.create (if size < 1 then 128 else size)
    let replace t ~key ~data = Hashtbl.replace t key data
    let iter t ~f = Hashtbl.iter (fun key data -> f ~key ~data) t
    let is_empty t = Hashtbl.length t = 0

    let add t ~key ~data =
      if Hashtbl.mem t key then `Duplicate
      else (
        Hashtbl.add t key data;
        `Ok
      )

    let add_exn t ~key ~data =
      match add t ~key ~data with
      | `Ok -> ()
      | `Duplicate ->
        raise (invalid_argf "Duplicate key: %s"
                 (Lazy.force (pretty (module K) key)))

    let keys t =
      let acc = ref [] in
      Hashtbl.iter (fun k _ ->
          acc := k :: !acc
        ) t;
      List.rev !acc

    let map t ~f =
      let acc = create ~size:(Hashtbl.length t) () in
      Hashtbl.iter (fun k v ->
          Hashtbl.add acc k (f v)
        ) t;
      acc

    let fold t ~init ~f =
      let acc = ref init in
      Hashtbl.iter (fun key data ->
          acc := f ~key ~data !acc
        ) t;
      !acc

    let filter t ~f =
      let acc = create ~size:(Hashtbl.length t) () in
      Hashtbl.iter (fun key data ->
          if f ~key ~data then Hashtbl.add acc key data
        ) t;
      acc

    let find t key =
      try Some (Hashtbl.find t key)
      with Not_found -> None

    let add_multi t ~key ~data =
      match find t key with
      | None -> replace t ~key ~data:[data]
      | Some l -> replace t ~key ~data:(data :: l)

  end

end

module Map = struct
  module type S = sig
    include Dictionary
    val empty: 'a t
    val add: 'a t -> key:key -> data:'a -> 'a t
    val remove: 'a t -> key -> 'a t
    val keys: 'a t -> key list
    module Lwt: sig
      val merge: 'v1 t ->'v2 t ->
        f:(key:key -> [ `Both of 'v1 * 'v2 | `Left of 'v1 | `Right of 'v2 ]
           -> 'v3 option Lwt.t) -> 'v3 t Lwt.t
      val iter2: 'v1 t -> 'v2 t ->
        f:(key:key ->data:[ `Both of 'v1 * 'v2 | `Left of 'v1 | `Right of 'v2 ]
           -> unit Lwt.t) -> unit Lwt.t
    end
  end
  module Make (K: I0) = struct

    module Map = Map.Make(K)

    let mem t k = Map.mem k t
    let add t ~key ~data = Map.add key data t
    let remove t k = Map.remove k t
    let empty = Map.empty
    let filter t ~f = Map.filter (fun key data -> f ~key ~data) t
    let map t ~f = Map.map f t
    let is_empty = Map.is_empty
    let keys t = List.map ~f:fst (Map.bindings t)
    let iter t ~f = Map.iter (fun key data -> f ~key ~data) t

    let find t k =
      try Some (Map.find k t)
      with Not_found -> None

    let fold t ~init ~f =
      let acc = ref init in
      Map.iter (fun key data ->
          acc := f ~key ~data !acc
        ) t;
      !acc

    let iter2 ~f t1 t2 =
      let rec aux l1 l2 = match l1, l2 with
        | [], t -> List.iter ~f:(fun (key, v) -> f ~key ~data:(`Right v)) t
        | t, [] -> List.iter ~f:(fun (key, v) -> f ~key ~data:(`Left v)) t
        | (k1,v1)::t1, (k2,v2)::t2 ->
          match K.compare k1 k2 with
          | 0 ->
            f ~key:k1 ~data:(`Both (v1, v2));
            aux t1 t2
          | x -> if x < 0 then (
              f ~key:k1 ~data:(`Left v1);
              aux t1 l2
            ) else (
              f ~key:k2 ~data:(`Right v2);
              aux l1 t2
            )
      in
      aux (Map.bindings t1) (Map.bindings t2)

    module L = struct

      type 'a t = 'a Map.t
      module K = K

      let to_alist t =
        let acc = ref [] in
        Map.iter (fun k v ->
            acc := (k, v) :: !acc
          ) t;
        List.rev !acc

      let of_alist alist =
        let result = ref None in
        try
          let map = List.fold_left ~f:(fun t (key,data) ->
              if mem t key then (
                result := Some (`Duplicate_key key);
                raise Exit
              ) else add t ~key ~data
            ) ~init:Map.empty alist
          in
          `Ok map
        with Exit ->
          match !result with
          | Some x -> x
          | None   -> assert false

      let of_alist_exn alist =
        match of_alist alist with
        | `Ok x -> x
        | `Duplicate_key k -> raise (
            invalid_argf "Map.of_alist_exn: duplicate key %s"
              (Sexplib.Sexp.to_string (K.to_sexp k))
          )

    end

    include AListLike1(L)
    type key = Map.key

    module Lwt = struct
      open Lwt
      let iter2 m1 m2 ~f =
        let m3 = ref [] in
        iter2 ~f:(fun ~key ~data ->
            m3 := f ~key ~data :: !m3
          ) m1 m2;
        Lwt_list.iter_p
          (fun b -> b >>= fun () -> return_unit) (List.rev !m3)

      let merge m1 m2 ~f =
        let l3 = ref [] in
        let f ~key ~data =
          f ~key data >>= function
          | None   -> return_unit
          | Some v -> l3 := (key, v) :: !l3; return_unit
        in
        iter2 m1 m2 ~f >>= fun () ->
        let m3 = L.of_alist_exn !l3 in
        return m3

    end
  end
end

module Lwt_stream = struct

  include Lwt_stream

  open Lwt

  let lift s =
    let (stream: 'a Lwt_stream.t option ref) = ref None in
    let rec get () =
      match !stream with
      | Some s -> Lwt_stream.get s
      | None   ->
        s >>= fun s ->
        stream := Some s;
        get ()
    in
    Lwt_stream.from get

end
