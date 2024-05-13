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

type (_, _) eq = Refl : ('a, 'a) eq

module Typ = struct
  type 'a s = ..
  type 'a t = { s : 'a s; eq : 'b. 'b s -> ('a, 'b) eq option }

  let create (type a) () : a t =
    let open struct
      type _ s += S : a s

      let eq : type b. b s -> (a, b) eq option = function
        | S -> Some Refl
        | _ -> None
    end in
    { s = S; eq }

  let equal a b = a.eq b.s
end

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
  typename : string;
  to_string : 'a -> string;
  of_string : string -> ('a, [ `Msg of string ]) result;
  of_json_string : string -> ('a, [ `Msg of string ]) result;
  default : 'a;
  typ : 'a Typ.t;
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

  let v name = { name; keys = M.empty }
  let name { name; _ } = name
  let update spec name k = spec.keys <- M.add name k spec.keys
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

let key' ?docs ?docv ?doc ?typ ~spec ~typename ~to_string ~of_string
    ~of_json_string name default =
  let () =
    String.iter
      (function
        | '-' | '_' | 'a' .. 'z' | '0' .. '9' -> ()
        | _ -> raise @@ Invalid_argument name)
      name
  in
  match Spec.find_key spec name with
  | Some _ -> Fmt.invalid_arg "duplicate key: %s" name
  | _ ->
      let typ = match typ with Some typ -> typ | None -> Typ.create () in
      let to_univ, of_univ = Univ.create () in
      let k =
        {
          name;
          to_string;
          of_json_string;
          of_string;
          default;
          typename;
          typ;
          to_univ;
          of_univ;
          doc;
          docv;
          docs;
        }
      in
      Spec.update spec name (K k);
      k

let key ?docs ?docv ?doc ?typ ~spec name ty default =
  let to_string = Type.to_string ty in
  let typename =
    Fmt.str "%a" Type.pp_ty ty |> Astring.String.filter (fun c -> c <> '\n')
  in
  let of_string = Type.of_string ty in
  let of_json_string = Type.of_json_string ty in
  key' ?docs ?docv ?doc ?typ ~spec ~typename ~to_string ~of_json_string
    ~of_string name default

let name t = t.name
let doc t = t.doc
let docv t = t.docv
let docs t = t.docs
let typename t = t.typename
let of_string t = t.of_string
let of_json_string t = t.of_json_string
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

let find_key : type a. t -> string -> a Typ.t -> a =
 fun ((spec, _) as t) name typ ->
  match Spec.find_key spec name with
  | Some (K k) -> (
      match Typ.equal k.typ typ with
      | Some Refl -> get t k
      | None -> raise Not_found)
  | None -> raise Not_found

let keys (_, conf) = M.to_seq conf |> Seq.map (fun (k, _) -> k)
let with_spec (_, conf) spec = (spec, conf)

let to_strings (_, conf) =
  conf
  |> M.to_seq
  |> Seq.map (fun (K k, v) ->
         ( k.name,
           match k.of_univ v with
           | Some v -> k.to_string v
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
  key ~spec ~docv:"ROOT" ~doc:"The location of the Irmin store on disk."
    ~docs:"COMMON OPTIONS" "root"
    Type.(string)
    "."

let find_root (spec, d) : string option =
  match Spec.find_key spec "root" with
  | None -> None
  | Some (K k) -> (
      let v = find (spec, d) k in
      match v with None -> None | Some v -> Some (k.to_string v))
