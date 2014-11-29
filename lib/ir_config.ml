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

type univ = {
  v      : exn;
  to_sexp: unit Tc.to_sexp;
  to_json: unit Tc.to_json;
  size_of: unit Tc.size_of;
  write  : unit Tc.writer;
  hash   : unit Tc.hash;
  compare: exn -> int;
  equal  : exn -> bool;
}

let univ (type s) (module M: Tc.S0 with type t = s) =
  let module E = struct exception E of s end in
  let create t = {
    v = E.E t;
    to_sexp = (fun () -> M.to_sexp t);
    to_json = (fun () -> M.to_json t);
    size_of = (fun () -> M.size_of t);
    write   = (fun () -> M.write t);
    hash    = (fun () -> M.hash t);
    compare = (function
        | E.E x -> M.compare t x
        | e     -> Pervasives.compare (E.E t) e);
    equal = (function
        | E.E x -> M.equal t x
        | _     -> false);
  } in
  let module T = struct
    type t = univ
    let hash t = t.hash ()
    let to_sexp t = t.to_sexp ()
    let to_json t = t.to_json ()
    let size_of t = t.size_of ()
    let write t   = t.write ()
    let of_json j = create (M.of_json j)
    let read b = create (M.read b)
    let compare t y = t.compare y.v
    let equal t y = t.equal y.v
  end in
  (fun x -> create x),
  (function { v = E.E x; _ } -> Some x | _ -> None),
  (module T: Tc.S0 with type t = univ)

type t = (string * univ) list

let to_dict x = x
let of_dict x = x

let find config k f =
  try f (List.assoc k (to_dict config))
  with Not_found -> None

let find_bool config k f ~default =
  match find config k f with
  | None   -> default
  | Some b -> b
