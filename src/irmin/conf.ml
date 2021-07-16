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

module Spec = struct
  type t = { name : string; keys : (string, k) Hashtbl.t }

  let all = Hashtbl.create 8

  let v name =
    let keys = Hashtbl.create 8 in
    if Hashtbl.mem all name then
      Fmt.failwith "Config spec already exists: %s" name;
    let x = { name; keys } in
    Hashtbl.replace all name x;
    x

  let name { name; _ } = name
  let update { keys; _ } name k = Hashtbl.replace keys name k
  let list () = Hashtbl.to_seq all
  let find name = Hashtbl.find_opt all name
  let find_key spec name = Hashtbl.find_opt spec.keys name
  let keys spec = Hashtbl.to_seq_values spec.keys
  let clone spec = { spec with keys = Hashtbl.copy spec.keys }

  let join dest src =
    let dest = clone dest in
    let name = ref dest.name in
    List.iter
      (fun spec ->
        if dest.name = spec.name then ()
        else
          let () = name := !name ^ "-" ^ spec.name in
          Hashtbl.add_seq dest.keys (Hashtbl.to_seq spec.keys))
      src;
    { dest with name = !name }
end

type t = Spec.t * Univ.t M.t

let spec = fst

let key ?docs ?docv ?doc ~spec name ty default =
  let () =
    String.iter
      (function
        | '-' | '_' | 'a' .. 'z' | '0' .. '9' -> ()
        | _ -> raise @@ Invalid_argument name)
      name
  in
  let to_univ, of_univ = Univ.create () in
  let k = { name; ty; default; to_univ; of_univ; doc; docv; docs } in
  Spec.update spec name (Key k);
  k

let name t = t.name
let doc t = t.doc
let docv t = t.docv
let docs t = t.docs
let ty t = t.ty
let default t = t.default
let empty spec = (spec, M.empty)
let singleton spec k v = (spec, M.singleton (Key k) (k.to_univ v))
let is_empty (_, t) = M.is_empty t
let mem (_, d) k = M.mem (Key k) d

let add (spec, d) k v =
  if Spec.find_key spec k.name |> Option.is_none then
    Fmt.failwith "invalid key, not found in spec: %s" k.name;
  (spec, M.add (Key k) (k.to_univ v) d)

let union (rs, r) (ss, s) =
  let spec = Spec.join rs [ ss ] in
  (spec, M.fold M.add r s)

let rem (s, d) k = (s, M.remove (Key k) d)
let find (_, d) k = try k.of_univ (M.find (Key k) d) with Not_found -> None
let uri = Type.(map string) Uri.of_string Uri.to_string

let get (_, d) k =
  try
    match k.of_univ (M.find (Key k) d) with
    | Some v -> v
    | None -> raise Not_found
  with Not_found -> k.default

let keys (_, conf) = M.to_seq conf |> Seq.map (fun (k, _) -> k)

(* ~root *)
let root spec =
  key ~spec ~docv:"ROOT" ~doc:"The location of the Irmin store on disk."
    ~docs:"COMMON OPTIONS" "root"
    Type.(string)
    "."
