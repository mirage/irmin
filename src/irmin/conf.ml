(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Daniel C. Bünzli
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
include Conf_intf

module Univ = struct
  type t = exn

  let create (type s) () =
    let module M = struct
      exception E of s option
    end in
    ((fun x -> M.E (Some x)), function M.E x -> x | _ -> None)
end

type 'a key = {
  name : string;
  doc : string option;
  docv : string option;
  docs : string option;
  ty : 'a Type.t;
  default : 'a;
  to_univ : 'a -> Univ.t;
  of_univ : Univ.t -> 'a option;
}

type k = Key : 'a key -> k

module M = Map.Make (struct
  type t = k

  let compare (Key a) (Key b) = String.compare a.name b.name
end)

type t = Univ.t M.t

let name t = t.name
let doc t = t.doc
let docv t = t.docv
let docs t = t.docs
let ty t = t.ty
let default t = t.default

module Make () = struct
  type t = Univ.t M.t
  type nonrec 'a key = 'a key
  type nonrec k = k

  let all = Hashtbl.create 8

  let key ?docs ?docv ?doc name ty default =
    let () =
      String.iter
        (function
          | '-' | '_' | 'a' .. 'z' | '0' .. '9' -> ()
          | _ -> raise @@ Invalid_argument name)
        name
    in
    let to_univ, of_univ = Univ.create () in
    let k = { name; ty; default; to_univ; of_univ; doc; docv; docs } in
    Hashtbl.replace all name (Key k);
    k

  let name t = t.name
  let doc t = t.doc
  let docv t = t.docv
  let docs t = t.docs
  let ty t = t.ty
  let default t = t.default
  let empty = M.empty
  let singleton k v = M.singleton (Key k) (k.to_univ v)
  let is_empty = M.is_empty
  let mem d k = M.mem (Key k) d
  let add d k v = M.add (Key k) (k.to_univ v) d
  let union r s = M.fold M.add r s
  let rem d k = M.remove (Key k) d
  let find d k = try k.of_univ (M.find (Key k) d) with Not_found -> None
  let uri = Type.(map string) Uri.of_string Uri.to_string

  let get d k =
    try
      match k.of_univ (M.find (Key k) d) with
      | Some v -> v
      | None -> raise Not_found
    with Not_found -> k.default

  let find_key d name =
    M.find_first_opt (fun (Key k) -> String.equal name k.name) d
    |> Option.map fst

  let list_keys conf = M.to_seq conf |> Seq.map (fun (k, _) -> k)

  (* ~root *)
  let root () =
    key ~docv:"ROOT" ~doc:"The location of the Git repository root."
      ~docs:"COMMON OPTIONS" "root"
      Type.(option string)
      None

  let k x = Key x
end

module type S = S with type t = t and type 'a key = 'a key and type k = k

module Extend (S : S) = struct
  include Make ()

  let () = Hashtbl.iter (fun k (Key v) -> Hashtbl.replace all k (Key v)) S.all
end
