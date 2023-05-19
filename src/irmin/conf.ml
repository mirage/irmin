(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open! Import

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

type k = K : 'a key -> k

module M = Map.Make (struct
  type t = k

  let compare (K a) (K b) = String.compare a.name b.name
end)

module Spec = struct
  module M = Map.Make (String)

  type t = { name : string; mutable keys : k M.t }

  let all = Hashtbl.create 8

  let v name =
    let keys = M.empty in
    if Hashtbl.mem all name then
      Fmt.failwith "Config spec already exists: %s" name;
    let x = { name; keys } in
    Hashtbl.replace all name x;
    x

  let name { name; _ } = name
  let update spec name k = spec.keys <- M.add name k spec.keys
  let list () = Hashtbl.to_seq_values all
  let find name = Hashtbl.find_opt all name
  let find_key spec name = M.find_opt name spec.keys
  let keys spec = M.to_seq spec.keys |> Seq.map snd
  let clone { name; keys } = { name; keys }

  let join dest src =
    let dest = clone dest in
    let name = ref dest.name in
    let keys =
      List.fold_left
        (fun acc spec ->
          if dest.name = spec.name then acc
          else
            let () = name := !name ^ "-" ^ spec.name in
            M.add_seq (M.to_seq spec.keys) acc)
        dest.keys src
    in
    { name = !name; keys }
end

type t = Spec.t * Univ.t M.t

let spec = fst

let key ?docs ?docv ?doc ?(allow_duplicate = false) ~spec name ty default =
  let () =
    String.iter
      (function
        | '-' | '_' | 'a' .. 'z' | '0' .. '9' -> ()
        | _ -> raise @@ Invalid_argument name)
      name
  in
  match Spec.find_key spec name with
  | Some _ when allow_duplicate = false ->
      Fmt.invalid_arg "duplicate key: %s" name
  | _ ->
      let to_univ, of_univ = Univ.create () in
      let k = { name; ty; default; to_univ; of_univ; doc; docv; docs } in
      Spec.update spec name (K k);
      k

let name t = t.name
let doc t = t.doc
let docv t = t.docv
let docs t = t.docs
let ty t = t.ty
let default t = t.default
let empty spec = (spec, M.empty)
let singleton spec k v = (spec, M.singleton (K k) (k.to_univ v))
let is_empty (_, t) = M.is_empty t
let mem (_, d) k = M.mem (K k) d

let validate_key spec k =
  match Spec.find_key spec k.name with
  | None -> Fmt.invalid_arg "invalid config key: %s" k.name
  | Some _ -> ()

let add (spec, d) k v =
  validate_key spec k;
  (spec, M.add (K k) (k.to_univ v) d)

let verify (spec, d) =
  M.iter (fun (K k) _ -> validate_key spec k) d;
  (spec, d)

let union (rs, r) (ss, s) =
  let spec = Spec.join rs [ ss ] in
  (spec, M.fold M.add r s)

let rem (s, d) k = (s, M.remove (K k) d)
let find (_, d) k = try k.of_univ (M.find (K k) d) with Not_found -> None
let uri = Type.(map string) Uri.of_string Uri.to_string

let get (_, d) k =
  try
    match k.of_univ (M.find (K k) d) with
    | Some v -> v
    | None -> raise Not_found
  with Not_found -> k.default

let keys (_, conf) = M.to_seq conf |> Seq.map (fun (k, _) -> k)
let with_spec (_, conf) spec = (spec, conf)

let to_strings (_, conf) =
  conf
  |> M.to_seq
  |> Seq.map (fun (K k, v) ->
         ( k.name,
           match k.of_univ v with
           | Some v -> Type.to_string k.ty v
           | None -> assert false ))

let pp ppf t =
  t |> to_strings |> List.of_seq |> Fmt.Dump.(list (pair string string)) ppf

let equal t1 t2 =
  t1 == t2
  || Seq.for_all2
       (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && String.equal v1 v2)
       (to_strings t1) (to_strings t2)

(* ~root *)
let root spec =
  key ~allow_duplicate:true ~spec ~docv:"ROOT"
    ~doc:"The location of the Irmin store on disk." ~docs:"COMMON OPTIONS"
    "root"
    Type.(string)
    "."

let find_root (spec, d) : string option =
  match Spec.find_key spec "root" with
  | None -> None
  | Some (K k) -> (
      let v = find (spec, d) k in
      match v with None -> None | Some v -> Some (Type.to_string k.ty v))
