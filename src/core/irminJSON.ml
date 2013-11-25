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

let debug fmt =
  IrminLog.debug "JSON" fmt

(* From http://erratique.ch/software/jsonm/doc/Jsonm.html#datamodel *)
type t =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of t list
  | `O of (string * t) list ]

exception Escape of ((int * int) * (int * int)) * Jsonm.error

let json_of_src ?encoding src =
  let dec d = match Jsonm.decode d with
    | `Lexeme l -> l
    | `Error e  -> raise (Escape (Jsonm.decoded_range d, e))
    | `End
    | `Await    -> assert false
  in
  let rec value v k d = match v with
    | `Os -> obj [] k d
    | `As -> arr [] k d
    | `Null
    | `Bool _
    | `String _
    | `Float _ as v -> k v d
    | _ -> assert false
  and arr vs k d = match dec d with
    | `Ae -> k (`A (List.rev vs)) d
    | v   -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with
    | `Oe     -> k (`O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _       -> assert false
  in
  let d = Jsonm.decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> `Error (r, e)

let of_stream (stream: string Lwt_stream.t): t Lwt_stream.t =
  let rec dec d = match Jsonm.decode d with
    | `Lexeme l -> return l
    | `Error e  -> fail (Escape (Jsonm.decoded_range d, e))
    | `Await    ->
      begin
        Lwt_stream.get stream >>= function
        | None    -> failwith "???"
        | Some str ->
          Jsonm.Manual.src d str 0 (String.length str);
          dec d
      end;
    | `End      -> assert false
  in
  let rec value v k d = match v with
    | `Os -> obj [] k d
    | `As -> arr [] k d
    | `Null
    | `Bool _
    | `String _
    | `Float _ as v -> k v d
    | _ -> assert false
  and arr vs k d =
    dec d >>= function
    | `Ae -> k (`A (List.rev vs)) d
    | v   -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d =
    dec d >>= function
    | `Oe     -> k (`O (List.rev ms)) d
    | `Name n ->
      dec d >>= fun l ->
      value l (fun v -> obj ((n, v) :: ms) k) d
    | _       -> assert false
  in
  let d = Jsonm.decoder `Manual in
  let get () =
    try
      dec d
      >>= fun l ->
      value l (fun v _ -> return v) d
      >>= fun json ->
      return (Some json)
    with
    | Escape _ -> return_none
  in
  Lwt_stream.from get

let of_buffer buf: t =
  let str = Buffer.contents buf in
  match json_of_src (`String str) with
  | `JSON j  -> j
  | `Error _ -> failwith "JSON.of_buffer"

let input str: t =
  debug "input %S" str;
  match json_of_src (`String str) with
  | `JSON j  -> j
  | `Error (_,e) ->
    Jsonm.pp_error Format.str_formatter e;
    debug "ERROR: %s" (Format.flush_str_formatter ());
    failwith "JSON.input"

let json_to_dst ~minify dst (json:t) =
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
  let rec value v k e = match v with
    | `A vs -> arr vs k e
    | `O ms -> obj ms k e
    | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
  and arr vs k e = enc e `As; arr_vs vs k e
  and arr_vs vs k e = match vs with
    | v :: vs' -> value v (arr_vs vs' k) e
    | [] -> enc e `Ae; k e
  and obj ms k e = enc e `Os; obj_ms ms k e
  and obj_ms ms k e = match ms with
    | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
    | [] -> enc e `Oe; k e
  in
  let e = Jsonm.encoder ~minify dst in
  let finish e = ignore (Jsonm.encode e `End) in
  match json with
  | `A _ | `O _ as json -> value json finish e
  | _ -> invalid_arg "invalid json text"

let to_buffer buf (json:t) =
  json_to_dst ~minify:true (`Buffer buf) json

let output t =
  let buf = Buffer.create 1024 in
  to_buffer buf t;
  Buffer.contents buf

(* unit *)
let of_unit () = `Null

let to_unit = function
  | `Null  -> ()
  | _      -> failwith "JSON.to_unit"

(* bool *)
let of_bool b = `Bool b

let to_bool (json:t) = match json with
  | `Bool b -> b
  | _       -> failwith "JSON.to_bool"

(* string *)
let of_string s = `String s

let to_string (json:t) = match json with
  | `String s -> s
  | j         -> failwith ("JSON.to_string: " ^ output j)

(* int *)
let of_int i = `String (string_of_int i)

let to_int j = int_of_string (to_string j)

(* list *)
let of_list fn = function
  | [] -> `Null
  | l  -> `A (List.map fn l)

let to_list fn (json:t) = match json with
  | `Null -> []
  | `A ks -> List.map fn ks
  | _     -> failwith "JSON.to_list"

(* string lists *)
let of_strings = of_list of_string

let to_strings = to_list to_string

(* options *)
let of_option fn = function
  | None   -> `Null
  | Some x -> fn x

let to_option fn (json:t) = match json with
  | `Null  -> None
  |  _     -> Some (fn json)

(* dict *)
let of_dict d : t = `O d

let to_dict = function
  | `O d -> d
  | _     -> failwith "JSON.to_dict"

(* pairs *)
let of_pair fk fv (k, v) =
  `A [fk k; fv v]

let to_pair fk fv (json:t) = match json with
  | `A [k; v] -> (fk k, fv v)
  | _         -> failwith "JSON.to_pair"
