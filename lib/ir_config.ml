(*
 * Copyright (c) 2014 Daniel C. Bünzli
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Univ = struct
  type t = exn
  let create (type s) () =
    let module M = struct exception E of s option end in
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)
end

type 'a key = {
  id     : int;
  to_univ: 'a -> Univ.t;
  of_univ: Univ.t -> 'a option;
  name   : string;
  doc    : string option;
  docv   : string option;
  docs   : string option;
  tc     : 'a Tc.t;
  default: 'a;
}

let name t = t.name
let doc t = t.doc
let docv t = t.docv
let docs t = t.docs
let tc t = t.tc
let default t = t.default

let key ?docs ?docv ?doc name tc default =
  let to_univ, of_univ = Univ.create () in
  let id = Oo.id (object end) in
  { id; to_univ; of_univ; name; docs; docv; doc; tc; default }

module Id = struct
  type t = int
  let compare (x:int) (y:int) = compare x y
end

module M = Map.Make (Id)

type t = Univ.t M.t

let empty = M.empty
let is_empty = M.is_empty
let mem d k = M.mem k.id d
let add d k v = M.add k.id (k.to_univ v) d
let rem d k = M.remove k.id d
let find d k = try k.of_univ (M.find k.id d) with Not_found -> None
let get d k =
  try match k.of_univ (M.find k.id d) with Some v -> v | None -> raise Not_found
  with Not_found -> raise Not_found

(* ~root *)
let root =
  key
    ~docv:"ROOT"
    ~doc:"The location of the Git repository root."
    "root" (Tc.option Tc.string) None
