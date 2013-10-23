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

module Watch = struct

  type watch = int

  type path = string list

  let c = ref 0

  let add tag path fn =
    let w = !c in
    incr c;
    let ws =
      try Hashtbl.find watches tag
      with Not_found -> [] in
    Hashtbl.add watches tag ((w, path, fn) :: ws);
    return w

  let remove tag path watch =
    try
      let ws = Hashtbl.find watches tag in
      let ws = List.filter (fun (w,p,_) -> w<>watch && p<>path) ws in
      return (match ws with
          | [] -> Hashtbl.remove watches tag
          | _  -> Hashtbl.replace watches tag ws
        )
    with Not_found ->
      return_unit

  let list () =
    let l = Hashtbl.fold
        (fun tag ws acc ->
           List.fold_left (fun acc (watch, path, _) ->
               (tag, path, watch) :: acc
             ) acc ws
        ) watches [] in
    return l

end
