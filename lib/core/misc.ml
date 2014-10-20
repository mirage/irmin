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

let show (type t) (module S: I0 with type t = t) t =
  lazy (
    Sexplib.Sexp.to_string_hum (S.to_sexp t)
  )

let shows (type t) (module S: I0 with type t = t) xs =
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

  let chop_prefix t ~prefix =
    let lt = String.length t in
    let lp = String.length prefix in
    if lt < lp then None else
      let p = String.sub t 0 lp in
      if String.compare p prefix <> 0 then None
      else Some (String.sub t lp (lt - lp))

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

let read_all (type t) (module S: I0 with type t = t) buf =
  match S.read buf with
  | None -> None
  | Some (_, t) ->
    (* FIXME: assert that the buffer is empty? *)
    Some t

let write_all (type t) (module S: I0 with type t = t) t =
  let buf = Cstruct.create (S.size_of t) in
  let buf = S.write t buf in
  (* FIXME: assert len=off *)
  buf

(* XXX: review performance *)
let read_string m str = read_all m (Cstruct.of_string str)
let write_string m t = Cstruct.to_string (write_all m t)

module Unit = I0(struct type t = unit with sexp, bin_io, compare end)

module Int64 = struct
  include I0(struct type t = int64 with sexp, bin_io, compare end)
  let (+) = Int64.add
  let to_string = Int64.to_string
  let of_string = Int64.of_string
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

let list_partition_map t ~f =
  let rec aux fst snd = function
    | []   -> List.rev fst, List.rev snd
    | h::t ->
      match f h with
      | `Fst x -> aux (x :: fst) snd t
      | `Snd x -> aux fst (x :: snd) t
  in
  aux [] [] t

let list_pretty f = function
  | [] -> "{}"
  | l  ->
    let buf = Buffer.create 1024 in
    let len = ref (List.length l - 1) in
    Buffer.add_string buf "{ ";
    List.iter (fun e ->
        Buffer.add_string buf (f e);
        if !len > 0 then Buffer.add_string buf ", ";
        decr len
      ) l;
    Buffer.add_string buf " }";
    Buffer.contents buf

(** Persistent Maps. *)
module type MAP = sig
  include Map.S
  include I1 with type 'a t := 'a t
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> 'a t
  val keys: 'a t -> key list
  val add_multi: key -> 'a -> 'a list t -> 'a list t
end
module Map (S: I0) = struct

end


(** Persistent Sets. *)
module type SET = sig
  include Set.S
  include I0 with type t := t
  val of_list: elt list -> t
  val to_list: t -> elt list
end
module Set (K: I0) = struct
  module Set = Set.Make(K)
  let to_list t =
    let l = ref [] in
    Set.iter (fun k -> l := k :: !l) t;
    List.rev !l
  let of_list l =
    let t = ref Set.empty in
    List.iter (fun k -> t := Set.add k !t) l;
    !t
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
