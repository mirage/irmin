(*
 * Copyright (c) 2017 Daniel C. Bünzli
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type 'a parser = string -> ('a, [`Msg of string]) result
type 'a printer = 'a Fmt.t
type 'a converter = 'a parser * 'a printer

let parser (p, _) = p
let printer (_, p) = p

let str = Printf.sprintf
let quote s = str "`%s'" s

module Err = struct
  let alts = function
  | [a; b] -> str "either %s or %s" a b
  | alts -> str "one of: %s" (String.concat ", " alts)

  let invalid kind s exp = str "invalid %s %s, %s" kind (quote s) exp
  let invalid_val = invalid "value"
end

let bool =
  (fun s -> try Ok (bool_of_string s) with Invalid_argument _ ->
     Error (`Msg (Err.invalid_val s (Err.alts ["true"; "false"])))),
  Fmt.bool

let parse_with t_of_str exp s =
  try Ok (t_of_str s) with Failure _ -> Error (`Msg (Err.invalid_val s exp))

let int = parse_with int_of_string "expected an integer", Fmt.int

let string = (fun s -> Ok s), Fmt.string

let some (parse, print) =
  let none = "" in
  (fun s -> match parse s with Ok v -> Ok (Some v) | Error _ as e -> e),
  (fun ppf v -> match v with None -> Fmt.string ppf none| Some v -> print ppf v)

let uri =
  let parse s = Ok (Uri.of_string s) in
  let print pp u = Fmt.string pp (Uri.to_string u) in
  parse, print

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
  conv   : 'a converter;
  default: 'a;
}

let name t = t.name
let doc t = t.doc
let docv t = t.docv
let docs t = t.docs
let conv t = t.conv
let default t = t.default

let key ?docs ?docv ?doc name conv default =
  let () =
    String.iter
      (function
        | '-' | '_' | 'a' .. 'z' | '0' .. '9' -> ()
        | _ -> raise @@ Invalid_argument name)
      name
  in
  let to_univ, of_univ = Univ.create () in
  let id = Oo.id (object end) in
  {id; to_univ; of_univ; name; docs; docv; doc; conv; default}

module Id = struct
  type t = int
  let compare (x:int) (y:int) = compare x y
end

module M = Map.Make (Id)

type t = Univ.t M.t

let empty = M.empty
let singleton k v = M.singleton k.id (k.to_univ v)
let is_empty = M.is_empty
let mem d k = M.mem k.id d
let add d k v = M.add k.id (k.to_univ v) d
let union r s = M.fold M.add r s
let rem d k = M.remove k.id d
let find d k = try k.of_univ (M.find k.id d) with Not_found -> None
let get d k =
  try match k.of_univ (M.find k.id d) with
    | Some v -> v
    | None   -> raise Not_found
  with Not_found ->
    k.default

(* ~root *)
let root =
  key
    ~docv:"ROOT"
    ~doc:"The location of the Git repository root."
    ~docs:"COMMON OPTIONS"
    "root" (some string) None
