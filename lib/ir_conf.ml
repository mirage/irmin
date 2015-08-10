(*
 * Copyright (c) 2015 Daniel C. Bünzli
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type 'a parser = string -> [ `Error of string | `Ok of 'a ]
type 'a printer = Format.formatter -> 'a -> unit
type 'a converter = 'a parser * 'a printer

let parser (p, _) = p
let printer (_, p) = p

let pr = Format.fprintf
let str = Printf.sprintf
let pr_str = Format.pp_print_string
let quote s = str "`%s'" s

module Err = struct
  let alts = function
  | [a; b] -> str "either %s or %s" a b
  | alts -> str "one of: %s" (String.concat ", " alts)

  let invalid kind s exp = str "invalid %s %s, %s" kind (quote s) exp
  let invalid_val = invalid "value"

  let element kind s exp = str "invalid element in %s (`%s'): %s" kind s exp
  let sep_miss sep s = invalid_val s (str "missing a `%c' separator" sep)
				   
end

let split_left sep s =
  try
    let i = String.index s sep in
    let len = String.length s in
    Some ((String.sub s 0 i), (String.sub s (i + 1) (len - i - 1)))
	 with Not_found -> None
	       
let pair ?(sep = ',') (pa0, pr0) (pa1, pr1) =
  let parser s = match split_left sep s with
    | None -> `Error (Err.sep_miss sep s)
    | Some (v0, v1) ->
       match pa0 v0, pa1 v1 with
       | `Ok v0, `Ok v1 -> `Ok (v0, v1)
       | `Error e, _ | _, `Error e -> `Error (Err.element "pair" s e)
  in
  let printer ppf (v0, v1) = pr ppf "%a%c%a" pr0 v0 sep pr1 v1 in
  parser, printer

	    
	    
let bool =
  (fun s -> try `Ok (bool_of_string s) with Invalid_argument _ ->
     `Error (Err.invalid_val s (Err.alts ["true"; "false"]))),
  Format.pp_print_bool

let parse_with t_of_str exp s =
  try `Ok (t_of_str s) with Failure _ -> `Error (Err.invalid_val s exp)

let int =
  parse_with int_of_string "expected an integer", Format.pp_print_int

let string = (fun s -> `Ok s), pr_str

let some (parse, print) =
  let none = "" in
  (fun s -> match parse s with `Ok v -> `Ok (Some v) | `Error _ as e -> e),
  (fun ppf v -> match v with None -> pr_str ppf none| Some v -> print ppf v)

let uri =
  let parse s = `Ok (Uri.of_string s) in
  let print pp u = Format.pp_print_string pp (Uri.to_string u) in
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
  let to_univ, of_univ = Univ.create () in
  let id = Oo.id (object end) in
  { id; to_univ; of_univ; name; docs; docv; doc; conv; default }

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
    "root" (some string) None
