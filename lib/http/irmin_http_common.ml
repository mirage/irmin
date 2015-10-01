(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

module Log = Log.Make(struct let section = "HTTP" end)

let ok_or_duplicated_branch_id =
  let module M = struct
    type t = [ `Ok | `Duplicated_branch | `Empty_head ]
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash

    let to_string = function
      | `Ok -> "ok"
      | `Duplicated_branch -> "duplicated-tag"
      | `Empty_head -> "empty-head"

    let of_string = function
      | "ok" -> `Ok
      | "duplicated-tag" -> `Duplicated_branch
      | "empty-head" -> `Empty_head
      | s -> failwith ("parse error: " ^ s)

    let to_json t = `String (to_string t)

    let of_json = function
      | `String s -> of_string s
      | j -> Ezjsonm.parse_error j "ok_or_duplicated_tag"

    let read buf = of_string (Tc.String.read buf)
    let size_of t = Tc.String.size_of (to_string t)
    let write t = Tc.String.write (to_string t)

  end in
  (module M: Tc.S0 with type t = M.t)

let ok_or_error =
  let module M = struct
    type t  = [`Ok | `Error]
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)

    let to_string = function
      | `Ok    -> "ok"
      | `Error -> "error"

    let of_string = function
      | "ok"    -> `Ok
      | "error" -> `Error
      | s -> failwith ("parse error: " ^ s)

    let to_json s = `String (to_string s)
    let of_json = function
      | `String s -> of_string s
      | j -> Ezjsonm.parse_error j "ok_or_error"

    let read buf = of_string (Tc.String.read buf)
    let size_of t = Tc.String.size_of (to_string t)
    let write t = Tc.String.write (to_string t)

  end in
  (module M: Tc.S0 with type t = M.t)

let lca (type a) (head: a Tc.t) =
  let (module Head: Tc.S0 with type t = a) = head in
  let module M = struct
    module HL = Tc.List(Head)
    type t = [`Ok of Head.t list | `Max_depth_reached | `Too_many_lcas]
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)

    let to_json = function
      | `Ok x -> `O ["ok", HL.to_json x]
      | `Max_depth_reached -> `A [`String "max-depth-reached" ]
      | `Too_many_lcas -> `A [`String "too-many-lcas"]

    let of_json = function
      | `O [ "ok", j ] -> `Ok (HL.of_json j)
      | `A [`String "max-depth-reached" ] -> `Max_depth_reached
      | `A [`String "too-many-lcas"] -> `Too_many_lcas
      | j -> Ezjsonm.parse_error j "LCA.of_json"

    module OK = Tc.Pair(Tc.String) (Tc.List(Head))

    let to_string = function
      | `Max_depth_reached -> "max-depth-reached"
      | `Too_many_lcas -> "too-many-lcas"
      | `Ok l -> Tc.write_string (module OK) ("ok", l)

    let of_string s = match s with
      | "max-depth-reached" -> `Max_depth_reached
      | "too-many-lcas"     -> `Too_many_lcas
      | s -> let ok, l = Tc.read_string (module OK) s in
        if ok <> "ok" then failwith ("parse error: " ^ s)
        else `Ok l

    let read buf = of_string (Tc.String.read buf)
    let size_of t = Tc.String.size_of (to_string t)
    let write t = Tc.String.write (to_string t)

  end in
  (module M: Tc.S0 with type t = M.t)

let start_stream = "start"
let stop_stream = "stop"
let irmin_version = "X-IrminVersion"
let content_type_header = "Content-type"
let application_json = "application/json"
let application_octet_stream = "application/octet-stream"

type ct = [ `Json | `Raw ]

let ct_of_header h =
  let s = Cohttp.Header.get h content_type_header in
  if s = Some application_json then `Json
  else if s = Some application_octet_stream then `Raw
  else `Json

let html_headers =
  Cohttp.Header.of_list []
let json_headers =
  Cohttp.Header.of_list [content_type_header, application_json]
let raw_headers  =
  Cohttp.Header.of_list [content_type_header, application_octet_stream]

let header_of_ct = function
  | `Json -> application_json
  | `Raw  -> application_octet_stream

let string_of_ct = function
  | `Json -> "json"
  | `Raw  -> "raw"

let ct_of_string = function
  | "json" -> Some `Json
  | "raw"  -> Some `Raw
  | _      -> None

let truncate s n =
  if String.length s > n then String.sub s 0 n ^ "[..]" else s

type contents = Json of Ezjsonm.value | Raw of string

let ct_of_contents = function
  | None          -> `Json
  | Some (Json _) -> `Json
  | Some (Raw _)  -> `Raw

let raw_contents tc v = Raw (Tc.write_string tc v)
let json_contents tc v = Json (Tc.to_json tc v)
let json j = Json j

let contents m = function
  | Json j -> Tc.of_json m j
  | Raw r  -> Tc.read_string m r

module Request = struct

  type t = Irmin.task * contents option

  module T = Tc.Pair (Irmin.Task) (Tc.Option (Tc.String))

  let write task body = Tc.write_string (module T) (task, body)
  let read buf = Tc.read_string (module T) buf

  let to_string ct (task, body) = match ct, body with
    | _   , Some (Raw r)  -> write task (Some r)
    | `Raw, None          -> write task None
    | _   , Some (Json j) ->
      let task = Irmin.Task.to_json task in
      let json = `O [ ("task", task); ("params", j) ] in
      Ezjsonm.to_string json
    | `Json, None ->
      let task = Irmin.Task.to_json task in
      let json = `O [ ("task", task) ] in
      Ezjsonm.to_string json

  let contents_of_json body =
    try match Ezjsonm.from_string body with
      | `O l ->
        let task = Irmin.Task.of_json (List.assoc "task" l) in
        let params =
          try Some (Json (List.assoc "params" l)) with Not_found -> None
        in
        task, params
      | j -> Ezjsonm.parse_error j "process: wrong parameters"
    with e ->
      let str =
        Printf.sprintf
          "process: not a valid JSON body %S [%s]" body (Printexc.to_string e)
      in
      failwith str

  let contents_of_raw body =
    let task, body = read body in
    task, match body with None -> None | Some r -> Some (Raw r)

  let of_string ct body = match ct with
    | `Json -> contents_of_json body
    | `Raw  -> contents_of_raw body

  let to_body ct t = Cohttp_lwt_body.of_string (to_string ct t)

  let of_body ~meth ct body =
    Cohttp_lwt_body.length body >>= fun (len, body) ->
    if len = 0L then Lwt.return_none
    else (
      Cohttp_lwt_body.to_string body >|= fun b ->
      Log.debug "process %s: length=%Ld content-type=%s body=%S"
        meth len (string_of_ct ct) (truncate b 40);
      Some (of_string ct b)
    )

end

module Response = struct

  exception Server_error of string
  exception Client_error of string

  type t = [`Error of exn | `Ok of contents]

  let raise_bad_version v =
    let v = match v with None -> "<none>" | Some v -> v in
    let err = Printf.sprintf
        "bad server version: expecting {version: %S}, but got %S"
        Irmin.version v
    in
    raise (Client_error err)

  let error_of_json j =
    let string = Ezjsonm.decode_string_exn in
    match j with
    | `O ["invalid-argument", s] -> Invalid_argument (string s)
    | `O ["failure", s]          -> Failure (string s)
    | _ -> Server_error (string j)

  let json_version = Ezjsonm.encode_string Irmin.version

  let json_of_error e =
    let string = Ezjsonm.encode_string in
    let error = match e with
      | Invalid_argument s -> `O ["invalid-argument", string s]
      | Failure s          -> `O ["failure", string s]
      | e -> string (Printexc.to_string e)
    in
    let json = `O [ "error", error; "version", json_version ] in
    Ezjsonm.to_string json

  let of_json ~version json =
    if version then (
      let version =
        try Ezjsonm.find json ["version"] |> Ezjsonm.decode_string
        with Not_found -> None
      in
      if version <> Some Irmin.version then raise_bad_version version;
    );
    let error =
      try Some (Ezjsonm.find json ["error"])
      with Not_found -> None in
    let result =
      try Some (Ezjsonm.find json ["result"])
      with Not_found -> None in
    match error, result with
    | None  , None   -> `Error (Server_error "empty response")
    | Some e, None   -> `Error (error_of_json e)
    | None  , Some r -> `Ok (Json r)
    | Some _, Some _ -> `Error (Server_error "ambiguous response")

  let to_json json =
    let json = `O [ "result", json; "version", json_version ] in
    Ezjsonm.to_string json

  let error_of_raw s =
    match Stringext.cut s ~on:" " with
    | Some ("invalid-argument", s) -> Invalid_argument s
    | Some ("failure", s)          -> Failure s
    | None | Some _                -> Server_error s

  let raw_of_error = function
    | Invalid_argument s -> "invalid-argument " ^ s
    | Failure s -> "failure " ^ s
    | e -> Printexc.to_string e

  module T = Tc.Triple(Tc.String)(Tc.Bool)(Tc.String)
  let read = Tc.read_string (module T)
  let write = Tc.write_string (module T)

  let of_raw ~version r =
    let v, error, payload = read r in
    if version && v <> Irmin.version then raise_bad_version (Some v);
    if error then `Error (error_of_raw payload) else `Ok (Raw payload)

  let to_raw ~error raw = write (Irmin.version, error, raw)

  let of_string ~version ct s = match ct with
    | `Json -> of_json ~version (Ezjsonm.from_string s)
    | `Raw  -> of_raw ~version s

  let to_string ct = function
    | `Ok (Raw r)  -> raw_headers , to_raw ~error:false r
    | `Ok (Json j) -> json_headers, to_json j
    | `Error e     ->
      match ct with
      | `Json -> json_headers, json_of_error e
      | `Raw  -> raw_headers , to_raw ~error:true (raw_of_error e)

  let of_body ct b =
    Cohttp_lwt_body.to_string b >|= fun b ->
    let short = truncate b 40 in
    Log.debug "got response: %S (%s)" short (string_of_ct ct);
    of_string ~version:true ct b

  let to_body ct b =
    let h, b = to_string ct b in
    h, Cohttp_lwt_body.of_string b



end
