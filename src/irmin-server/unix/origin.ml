(*
 * Copyright (c) 2026 Tarides <contact@tarides.com>
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

type t = { scheme : string; host : string; port : int option }

let equal t t' = t.scheme = t'.scheme && t.host = t'.host && t.port = t'.port

let to_string { scheme; host; port } =
  match port with
  | None -> Printf.sprintf "%s://%s" scheme host
  | Some port -> Printf.sprintf "%s://%s:%d" scheme host port

let pp ppf t = Fmt.string ppf (to_string t)
let drop n s = String.sub s n (String.length s - n)
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

(* RFC 3986, section 3.1 *)
let is_scheme s =
  s <> ""
  && is_alpha s.[0]
  && String.for_all
       (fun c -> is_alpha c || is_digit c || c = '+' || c = '-' || c = '.')
       s

let is_label s =
  s <> ""
  && String.for_all (fun c -> is_alpha c || is_digit c || c = '-' || c = '_') s

(* The browsers read a host whose last label is a number as an IPv4 address
   (URL standard, "ends in a number"), and send it in dotted decimal. *)
let ends_in_number label =
  (label <> "" && String.for_all is_digit label)
  || String.length label >= 2
     && label.[0] = '0'
     && label.[1] = 'x'
     && String.for_all is_hex (drop 2 label)

(* A decimal number from 0 to 255, without a leading zero *)
let is_byte s =
  let n = String.length s in
  n > 0
  && n <= 3
  && String.for_all is_digit s
  && (n = 1 || s.[0] <> '0')
  && int_of_string s <= 255

let is_ipv4 labels = List.length labels = 4 && List.for_all is_byte labels

let rec find_double_colon s i =
  if i + 1 >= String.length s then None
  else if s.[i] = ':' && s.[i + 1] = ':' then Some i
  else find_double_colon s (i + 1)

(* The 8 groups of an IPv6 address, without an embedded IPv4 address *)
let ipv6_groups s =
  let ( let* ) = Option.bind in
  let groups = function
    | "" -> Some []
    | part ->
        let groups = String.split_on_char ':' part in
        if
          List.for_all
            (fun g ->
              g <> "" && String.length g <= 4 && String.for_all is_hex g)
            groups
        then Some (List.map (fun g -> int_of_string ("0x" ^ g)) groups)
        else None
  in
  match find_double_colon s 0 with
  | None ->
      let* groups = groups s in
      if List.length groups = 8 then Some groups else None
  | Some i ->
      let* left = groups (String.sub s 0 i) in
      let* right = groups (drop (i + 2) s) in
      let zeros = 8 - List.length left - List.length right in
      if zeros >= 1 then Some (left @ List.init zeros (fun _ -> 0) @ right)
      else None

(* The serialisation of the URL standard: lowercase, no leading zero, and the
   first longest run of at least two zero groups compressed as [::] *)
let ipv6_to_string groups =
  let groups = Array.of_list groups in
  let compress_start, compress_length =
    let best = ref (-1, 0) and start = ref (-1) in
    Array.iteri
      (fun i g ->
        if g = 0 then (
          if !start < 0 then start := i;
          let length = i - !start + 1 in
          if length > snd !best then best := (!start, length))
        else start := -1)
      groups;
    if snd !best >= 2 then !best else (-1, 0)
  in
  let buf = Buffer.create 39 in
  let rec loop i =
    if i < 8 then
      if i = compress_start then (
        Buffer.add_string buf (if i = 0 then "::" else ":");
        loop (i + compress_length))
      else (
        Buffer.add_string buf (Printf.sprintf "%x" groups.(i));
        if i < 7 then Buffer.add_char buf ':';
        loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

(* A lowercase host, written as the browsers send it: a name with an optional
   final dot, an IPv4 address in dotted decimal, or an IPv6 address in
   brackets in its canonical form. *)
let is_host s =
  let n = String.length s in
  if n > 0 && s.[0] = '[' then
    let address = String.sub s 1 (max 0 (n - 2)) in
    n > 2
    && s.[n - 1] = ']'
    &&
    match ipv6_groups address with
    | Some groups -> ipv6_to_string groups = address
    | None -> false
  else
    let labels = String.split_on_char '.' s in
    match List.rev labels with
    | last :: _ when ends_in_number last -> is_ipv4 labels
    | "" :: last :: _ when ends_in_number last -> false
    | "" :: (_ :: _ as rest) | rest -> List.for_all is_label rest

let port_of_string s =
  if
    s <> "" && String.length s <= 5 && s.[0] <> '0' && String.for_all is_digit s
  then
    let port = int_of_string s in
    if port <= 65535 then Some port else None
  else None

(* [host] or [host:port], where [host] may be an IPv6 address in brackets *)
let split_authority a =
  let host_end =
    let ipv6 = a <> "" && a.[0] = '[' in
    match String.index_opt a (if ipv6 then ']' else ':') with
    | Some i -> if ipv6 then i + 1 else i
    | None -> String.length a
  in
  let host = String.sub a 0 host_end in
  match drop host_end a with
  | "" -> Some (host, None)
  | port when port.[0] = ':' ->
      Option.map (fun port -> (host, Some port)) (port_of_string (drop 1 port))
  | _ -> None

let default_port = function
  | "http" -> Some 80
  | "https" -> Some 443
  | _ -> None

let parse s =
  let ( let* ) = Option.bind in
  let* i = String.index_opt s ':' in
  let scheme = String.sub s 0 i in
  let rest = drop (i + 1) s in
  let* authority =
    if String.length rest >= 2 && rest.[0] = '/' && rest.[1] = '/' then
      Some (drop 2 rest)
    else None
  in
  let authority =
    let n = String.length authority in
    if n > 0 && authority.[n - 1] = '/' then String.sub authority 0 (n - 1)
    else authority
  in
  let* host, port = split_authority authority in
  let host = String.lowercase_ascii host in
  if is_scheme scheme && is_host host then
    let scheme = String.lowercase_ascii scheme in
    let port = if port = default_port scheme then None else port in
    Some { scheme; host; port }
  else None

let of_string s =
  match parse s with
  | Some t -> Ok t
  | None ->
      Error (`Msg (Fmt.str "%S is not an origin, like https://example.org" s))
