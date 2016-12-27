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

open Ir_merge.OP

module Json = struct

  (* FIXME: we want maybe something more structured here. *)
  module Path = Ir_path.String_list

  module V = struct

    type t = Ezjsonm.value

    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)

    let to_json x = x
    let of_json x = x

    let to_string t = Ezjsonm.(to_string (wrap t))
    let of_string s = Ezjsonm.(unwrap (from_string s))

    let write t buf =
      let str = to_string t in
      let len = String.length str in
      Cstruct.blit_from_string str 0 buf 0 len;
      Cstruct.shift buf len

    let read buf =
      Mstruct.get_string buf (Mstruct.length buf)
      |> of_string

    let size_of t =
      let str = to_string t in
      String.length str

  end

  module T = struct

    type t = Ezjsonm.t

    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)

    let to_json = Ezjsonm.value
    let of_json = function
      | #Ezjsonm.t as x -> x
      | j -> Ezjsonm.parse_error j "Not a valid JSON document"

    let to_string t = Ezjsonm.(to_string t)
    let of_string s = Ezjsonm.(from_string s)

    let write t buf =
      let str = to_string t in
      let len = String.length str in
      Cstruct.blit_from_string str 0 buf 0 len;
      Cstruct.shift buf len

    let read buf =
      Mstruct.get_string buf (Mstruct.length buf)
      |> of_string

    let size_of t =
      let str = to_string t in
      String.length str

  end

  include T

  let rec merge_values path ~old x y =
    let old () =
      old () >>| function
      | None          -> ok None
      | Some (`O old) -> ok (Some old)
      | _             -> conflict "record"
    in
    match x, y with
    | `O x, `O y ->
      Ir_merge.alist (module Tc.String) (module V)
        (fun s -> merge_values_opt (Path.rcons path s))
        ~old:old x y
      >>| fun x -> ok (`O x)
    | _ -> conflict "JSON values"

  and merge_values_opt path ~old x y =
    Ir_merge.option (module V) (merge_values path) ~old x y >>| function
    | Some (`O []) -> ok None
    | x -> ok x

  let merge_t path ~old x y =
    let old () =
      old () >>| function
      | None          -> ok None
      | Some (`O old) -> ok (Some old)
      | _             -> conflict "record"
    in
    match x, y with
    | `O x, `O y ->
      Ir_merge.alist (module Tc.String) (module V)
        (fun s -> merge_values_opt (Path.rcons path s))
        ~old:old x y
      >>| fun x -> ok (`O x)
    | _ -> conflict "JSON documents"

  let merge path = Ir_merge.option (module T) (merge_t path)

end

module String = struct
  include Tc.String
  module Path = Ir_path.String_list
  let size_of t = String.length t
  let read buf = Mstruct.to_string buf
  let write t buf =
    let len = String.length t in
    Cstruct.blit_from_string t 0 buf 0 len;
    Cstruct.shift buf len
  let merge =
    let f = Ir_merge.default (module Tc.Option(Tc.String)) in
    fun _path -> f
end

module Cstruct = struct
  module S = struct
    type t = Cstruct.t

    let hash = Hashtbl.hash

    (* FIXME use Cstruct.compare *)
    let equal x y = Cstruct.to_bigarray x = Cstruct.to_bigarray y
    let compare x y =
      Pervasives.compare (Cstruct.to_bigarray x) (Cstruct.to_bigarray y)

    let to_json t = Cstruct.to_string t |> Ezjsonm.encode_string
    let of_json j = Cstruct.of_string (Ezjsonm.decode_string_exn j)
    let size_of t = Cstruct.len t
    let read b = Mstruct.to_cstruct b

    let write t buf =
      let len = Cstruct.len t in
      Cstruct.blit t 0 buf 0 len;
      Cstruct.shift buf len
  end
  include S
  module Path = Ir_path.String_list
  let merge =
    let f = Ir_merge.default (module Tc.Option(S)) in
    fun _path -> f

end

module Store
    (S: sig
       include Ir_s.AO_STORE
       module Key: Ir_s.HASH with type t = key
       module Val: Ir_s.CONTENTS with type t = value
     end) =
struct
  include S
  module Path = S.Val.Path
  let (>>=) = Lwt.bind

  let read_opt t = function
    | None   -> Lwt.return_none
    | Some k -> read t k

  let add_opt t = function
    | None -> Lwt.return_none
    | Some v -> add t v >>= fun k -> Lwt.return (Some k)

  let merge path t =
    Ir_merge.with_conflict
      (fun s -> Printf.sprintf "%s: %s" (Path.to_hum path) s)
      (Ir_merge.biject' (module Tc.Option(Key))
         (Val.merge path) (read_opt t) (add_opt t))

end
