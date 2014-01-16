(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  type t
  include Identifiable.S with type t := t
  val name: string
  val pretty: t -> string
  val of_json: Ezjsonm.t -> t
  val to_json: t -> Ezjsonm.t
end

module List (E: S) = struct

  module L = Log.Make(struct let section = "IO-LIST" end)

  module M = struct
    type t = E.t list
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "List"
    let name = E.name ^ "s"
  end
  include M
  include Identifiable.Make (M)

  let pretty ts =
    IrminMisc.pretty_list E.pretty ts

  let to_json t =
    `A (List.rev (List.rev_map ~f:E.to_json t))

  let of_json = function
    | `A l -> List.rev (List.rev_map ~f:E.of_json l)
    | _    -> Mstruct.parse_error "List.of_json"

end

module Option (E: S) = struct

  module L = Log.Make(struct let section = "IO-OPTION" end)

  module M = struct
    type t = E.t option
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Option"
    let name = E.name
  end
  include M
  include Identifiable.Make (M)

  let pretty = function
    | None   -> "<none>"
    | Some e -> E.pretty e

  let to_json = function
    | None   -> `Null
    | Some e -> E.to_json e

  let of_json = function
    | `Null -> None
    | j     -> Some (E.of_json j)

end

module Pair (K: S) (V: S) = struct

  module List = Core_kernel.Core_list

  module L = Log.Make(struct let section = "IO-PAIR" end)

  module M = struct
    type t = K.t * V.t
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Pair"
    let name = K.name ^ "-" ^ V.name
  end
  include M
  include Identifiable.Make (M)

  let pretty (key, value) =
    Printf.sprintf "%s:%s" (K.pretty key) (V.pretty value)

  let to_json (key, value) =
    `O [ (K.name, K.to_json key);
         (V.name, V.to_json value)]

  let of_json = function
    | `O l ->
      let key =
        try List.Assoc.find_exn l K.name
        with Not_found -> Mstruct.parse_error "Pair.of_json: missing %s" K.name
      in
      let value =
        try List.Assoc.find_exn l V.name
        with Not_found -> Mstruct.parse_error "Pair.of_json: missing %s" V.name
      in
      (K.of_json key, V.of_json value)
    | _ -> Mstruct.parse_error "Pair.of_json: not an object"

end

module String = struct

  module L = Log.Make(struct let section = "IO-string" end)

  include String

  let name = "string"

  let pretty t = t

  let to_json t =
    Ezjsonm.string t

  let of_json j =
    Ezjsonm.get_string j

end
