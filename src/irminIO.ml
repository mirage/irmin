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

(* From cstruct *)
type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create len ready = {
  buffer = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len;
  offset = 0;
  ready;
}

external unsafe_blit_string_to_bigstring
  : string -> int -> buffer -> int -> int -> unit
  = "caml_blit_string_to_bigstring" "noalloc"

external unsafe_blit_bigstring_to_string
  : buffer -> int -> string -> int -> int -> unit
  = "caml_blit_bigstring_to_string" "noalloc"

let poll t len =
  for_lwt i = t.offset to t.offset + len do
    t.ready i
  done

let set t len fn =
  lwt () = poll t len in
  fn t;
  t.offset <- t.offset + len;
  Lwt.return ()

let set_char t c =
  set t 1 (fun t ->
      EndianBigstring.BigEndian.set_char t.buffer t.offset c;
    )

let set_uint8 t c =
  set t 1 (fun t ->
      EndianBigstring.BigEndian.set_int8 t.buffer t.offset c;
    )

let set_uint16 t c =
  set t 2 (fun t ->
      EndianBigstring.BigEndian.set_int16 t.buffer t.offset c;
    )

let set_uint32 t c =
  set t 4 (fun t ->
      EndianBigstring.BigEndian.set_int32 t.buffer t.offset c;
    )

let set_uint64 t c =
  set t 8 (fun t ->
      EndianBigstring.BigEndian.set_int64 t.buffer t.offset c;
    )

let set_string t str =
  let len = String.length str in
  set t len (fun t ->
      unsafe_blit_string_to_bigstring str 0 t.buffer t.offset len;
    )

let get t n fn =
  lwt () = poll t n in
  let i = fn t in
  t.offset <- t.offset + n;
  Lwt.return i

let get_char t =
  get t 1 (fun t ->
      EndianBigstring.BigEndian.get_char t.buffer t.offset
    )

let get_uint8 t =
  get t 1 (fun t ->
      EndianBigstring.BigEndian.get_uint8 t.buffer t.offset
    )

let get_uint16 t =
  get t 2 (fun t ->
      EndianBigstring.BigEndian.get_uint16 t.buffer t.offset
    )

let get_uint32 t =
  get t 4 (fun t ->
      EndianBigstring.BigEndian.get_int32 t.buffer t.offset
    )

let get_uint64 t =
  get t 8 (fun t ->
      EndianBigstring.BigEndian.get_int64 t.buffer t.offset
    )

let get_string t len =
  let str = String.create len in
  get t len (fun t ->
      unsafe_blit_bigstring_to_string t.buffer t.offset str 0 len;
      str
    )

module OCamlList = List
module OCamlString = String

module List  (E: BASE) = struct

  type t = E.t list

  let pretty t =
    String.concat "\n" (OCamlList.rev (OCamlList.rev_map E.pretty t))

  let to_json t =
    `A (OCamlList.rev (OCamlList.rev_map E.to_json t))

  let of_json = function
    | `A l -> OCamlList.rev (List.rev_map E.of_json l)
    | _    -> failwith "MakeList.of_json"

  let sizeof l =
    List.fold_left (fun acc e ->
        acc + E.sizeof e
      ) 4 l

  let read buf =
    lwt keys = get_uint32 buf in
    let rec aux acc i =
      if i <= 0 then Lwt.return (OCamlList.rev acc)
      else
        lwt t = E.read buf in
        aux (t :: acc) (i-1) in
    aux [] (Int32.to_int keys)

  let write buf t =
    let len = Int32.of_int (List.length t) in
    lwt () = set_uint32 buf len in
    Lwt_list.iter_s (E.write buf) t

end

module Option (E: BASE) = struct

  type t = E.t option

  let pretty = function
    | None   -> "<none>"
    | Some e -> E.pretty e

  let to_json = function
    | None   -> `Null
    | Some e -> E.to_json e

  let of_json = function
    | `Null -> None
    | j     -> Some (E.of_json j)

  module L = List(E)

  let sizeof = function
    | None   -> 4
    | Some e -> 4 + E.sizeof e

  let read buf =
    lwt l = L.read buf in
    match l with
    | []  -> Lwt.return None
    | [e] -> Lwt.return (Some e)
    | _   -> failwith "Option.read"

  let write buf t =
    let l = match t with
      | None   -> []
      | Some e -> [e] in
    L.write buf l

end

module Pair (K: BASE) (V: BASE) = struct

  type t = K.t * V.t

  let pretty (key, value) =
    Printf.sprintf "%s:%s" (K.pretty key) (V.pretty value)

  let to_json (key, value) =
    `O [ ("tag", K.to_json key);
         ("key", V.to_json value)]

  let of_json = function
    | `O l ->
      let key =
        try OCamlList.assoc "tag" l
        with Not_found -> failwith "MakeProduct.of_json: missing tag" in
      let value =
        try OCamlList.assoc "key" l
        with Not_found -> failwith "MakeProduct.of_json: missing key" in
      (K.of_json key, V.of_json value)
    | _ -> failwith "Product.of_json: not an object"

  let sizeof (key, value) =
    K.sizeof key + V.sizeof value

  let read buf =
    lwt tag = K.read buf in
    lwt key = V.read buf in
    Lwt.return (tag, key)

  let write buf (key, value) =
    lwt () = K.write buf key in
    V.write buf value

end

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

module String  (S: STRINGABLE) = struct

  type t = S.t

  let pretty s =
    Printf.sprintf "%S" (S.to_string s)

  let to_json t =
    IrminJSON.of_string (S.to_string t)

  let of_json j =
    S.of_string (IrminJSON.to_string j)

  let sizeof s =
    4 + String.length (S.to_string s)

  let read buf =
    lwt len = get_uint32 buf in
    lwt str = get_string buf (Int32.to_int len) in
    Lwt.return (S.of_string str)

  let write buf t =
    let str = S.to_string t in
    let len = String.length str in
    lwt () = set_uint32 buf (Int32.of_int len) in
    set_string buf str

end

module Lwt_channel = struct

  type t = Lwt_unix.file_descr

  let close = Lwt_unix.close

  (* XXX: not optimized *)
  let ready _ _ = Lwt.return ()

  let read_string fd len =
    let buf = OCamlString.create len in
    let rec rread fd buf ofs len =
      lwt n = Lwt_unix.read fd buf ofs len in
      if n = 0 then raise End_of_file;
      if n < len then rread fd buf (ofs + n) (len - n) else Lwt.return () in
    lwt () = rread fd buf 0 len in
    Lwt.return buf

  let read_buf fd len =
    let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
    let rec rread fd buf ofs len =
      lwt n = Lwt_bytes.read fd buf ofs len in
      if n = 0 then raise End_of_file;
      if n < len then rread fd buf (ofs + n) (len - n) else Lwt.return () in
    lwt () = rread fd buf 0 len in
    let buffer = {
      buffer = buf;
      offset = 0;
      ready  = ready fd;
    } in
    Lwt.return buffer

  let write_string fd buf =
    let rec rwrite fd buf ofs len =
      lwt n = Lwt_unix.write fd buf ofs len in
      if n = 0 then raise End_of_file;
      if n < len then rwrite fd buf (ofs + n) (len - n) else Lwt.return () in
    rwrite fd buf 0 (OCamlString.length buf)

  let write_buf fd buf len =
    let rec rwrite fd buf ofs len =
      lwt n = Lwt_bytes.write fd buf ofs len in
      if n = 0 then raise End_of_file;
      if n < len then rwrite fd buf (ofs + n) (len - n) else Lwt.return () in
    rwrite fd buf.buffer buf.offset len

  let read_header fd =
    lwt str = read_string fd 4 in
    let len = EndianString.BigEndian.get_int32 str 0 in
    Lwt.return (Int32.to_int len)

  let write_header fd len =
    let str = OCamlString.create 4 in
    EndianString.BigEndian.set_int32 str 0 (Int32.of_int len);
    write_string fd str

end

module Channel (B: BASE) = struct

  include B

  type channel = Lwt_channel.t

  let read_fd fd =
    lwt len = Lwt_channel.read_header fd in
    lwt buf = Lwt_channel.read_buf fd len in
    B.read buf

  let write_fd fd t =
    let len = B.sizeof t in
    let buf = create len (Lwt_channel.ready fd) in
    lwt () = Lwt_channel.write_header fd len in
    Lwt_channel.write_buf fd buf len


end
