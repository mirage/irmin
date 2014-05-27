(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Core_kernel.Std

module type S = sig
  include Identifiable.S
  val of_json: Ezjsonm.t -> t
  val to_json: t -> Ezjsonm.t
end

module String = struct
  include String
  let of_json = IrminMisc.json_decode_exn
  let to_json = IrminMisc.json_encode
end

module Int = struct
  include Int
  let of_json = Ezjsonm.get_int
  let to_json = Ezjsonm.int
end

let rec json_of_sexp = function
  | Sexplib.Type.Atom x -> String.to_json x
  | Sexplib.Type.List l -> Ezjsonm.list json_of_sexp l

let rec sexp_of_json json =
  match IrminMisc.json_decode json with
  | Some s -> Sexplib.Type.Atom s
  | None   ->
    match json with
    | `A l -> Sexplib.Type.List (List.map ~f:sexp_of_json l)
    | _    -> failwith (sprintf "sexp_of_json: %s" (Ezjsonm.to_string json))

module Make (S: sig type t with sexp, compare end) = struct
  module S = struct
    type t = S.t with sexp, compare
    include Sexpable.To_stringable (struct type t = S.t with sexp end)
  end
  module B = Binable.Of_stringable(S)
  module M = struct
    include S
    let hash (t : t) = Hashtbl.hash t
    include B
    let module_name = "Ident"
  end
  include M
  include Identifiable.Make (M)
  let to_json t = json_of_sexp (sexp_of_t t)
  let of_json t = t_of_sexp (sexp_of_json t)
end
