(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Import

module Make (G : Git.S) (P : Irmin.Path.S) = struct
  module Key = Irmin.Hash.Make (G.Hash)
  module Raw = Git.Value.Make (G.Hash)
  module Path = P

  module V = struct
    type t = G.Value.Tree.t

    let type_eq = function `Tree -> true | _ -> false
    let to_git t = G.Value.tree t
    let of_git = function Git.Value.Tree t -> Some t | _ -> None
  end

  include Content_addressable.Check_closed (Content_addressable.Make (G) (V))

  module Val = struct
    module Metadata = Metadata

    type t = G.Value.Tree.t
    type metadata = Metadata.t [@@deriving irmin]
    type hash = Key.t [@@deriving irmin]
    type step = Path.step [@@deriving irmin]

    type value = [ `Node of hash | `Contents of hash * metadata ]
    [@@deriving irmin]

    let default = Metadata.default
    let of_step = Irmin.Type.to_string P.step_t

    let to_step str =
      match Irmin.Type.of_string P.step_t str with
      | Ok x -> x
      | Error (`Msg e) -> failwith e

    exception Exit of (step * value) list

    let list ?(offset = 0) ?length t =
      let t = G.Value.Tree.to_list t in
      let length = match length with None -> List.length t | Some n -> n in
      try
        List.fold_left
          (fun (i, acc) { Git.Tree.perm; name; node } ->
            if i < offset then (i + 1, acc)
            else if i >= offset + length then raise (Exit acc)
            else
              let name = to_step name in
              match perm with
              | `Dir -> (i + 1, (name, `Node node) :: acc)
              | `Commit -> (i + 1, acc) (* FIXME *)
              | #Metadata.t as p -> (i + 1, (name, `Contents (node, p)) :: acc))
          (0, []) t
        |> fun (_, acc) -> List.rev acc
      with Exit acc -> List.rev acc

    let find t s =
      let s = of_step s in
      let rec aux = function
        | [] -> None
        | x :: xs when x.Git.Tree.name <> s -> aux xs
        | { Git.Tree.perm; node; _ } :: _ -> (
            match perm with
            | `Dir -> Some (`Node node)
            | `Commit -> None (* FIXME *)
            | #Metadata.t as p -> Some (`Contents (node, p)))
      in
      aux (Git.Tree.to_list t)

    let remove t step = G.Value.Tree.remove ~name:(of_step step) t
    let is_empty = G.Value.Tree.is_empty
    let length t = G.Value.Tree.length t |> Int64.to_int

    let add t name value =
      let name = of_step name in
      let entry =
        match value with
        | `Node node -> Git.Tree.entry ~name `Dir node
        | `Contents (node, perm) ->
            Git.Tree.entry ~name (perm :> Git.Tree.perm) node
      in
      (* FIXME(samoht): issue in G.Value.Tree.add *)
      let entries = G.Value.Tree.to_list t in
      match List.find (fun e -> e.Git.Tree.name = name) entries with
      | exception Not_found -> Git.Tree.of_list (entry :: entries)
      | e ->
          let equal x y =
            x.Git.Tree.perm = y.Git.Tree.perm
            && x.name = y.name
            && G.Hash.equal x.node y.node
          in
          if equal e entry then t
          else
            let entries =
              List.filter (fun e -> e.Git.Tree.name <> name) entries
            in
            Git.Tree.of_list (entry :: entries)

    let empty : t = Git.Tree.of_list []

    let to_git perm (name, node) =
      G.Value.Tree.entry ~name:(of_step name) perm node

    let v alist =
      let alist =
        List.rev_map
          (fun (l, x) ->
            let v k = (l, k) in
            match x with
            | `Node n -> to_git `Dir (v n)
            | `Contents (c, perm) -> to_git (perm :> Git.Tree.perm) (v c))
          alist
      in
      (* Tree.of_list will sort the list in the right order *)
      G.Value.Tree.of_list alist

    let alist t =
      let mk_n k = `Node k in
      let mk_c k metadata = `Contents (k, metadata) in
      List.fold_left
        (fun acc -> function
          | { Git.Tree.perm = `Dir; name; node } ->
              (to_step name, mk_n node) :: acc
          | { Git.Tree.perm = `Commit; name; _ } ->
              (* Irmin does not support Git submodules; do not follow them,
                 just consider *)
              Log.warn (fun l -> l "skipping Git submodule: %s" name);
              acc
          | { Git.Tree.perm = #Metadata.t as perm; name; node; _ } ->
              (to_step name, mk_c node perm) :: acc)
        [] (G.Value.Tree.to_list t)
      |> List.rev

    module N = Irmin.Private.Node.Make (Key) (P) (Metadata)

    let to_n t = N.v (alist t)
    let of_n n = v (N.list n)
    let to_bin t = Raw.to_raw (G.Value.tree t)

    let encode_bin =
      Irmin.Type.stage @@ fun (t : t) k ->
      Log.debug (fun l -> l "Tree.encode_bin");
      k (to_bin t)

    let decode_bin =
      Irmin.Type.stage @@ fun buf off ->
      Log.debug (fun l -> l "Tree.decode_bin");
      match Raw.of_raw_with_header buf ~off with
      | Ok (Git.Value.Tree t) -> (String.length buf, t)
      | Ok _ -> failwith "wrong object kind"
      | Error _ -> failwith "wrong object"

    let size_of = Irmin.Type.Size.custom_dynamic ()
    let t = Irmin.Type.map ~bin:(encode_bin, decode_bin, size_of) N.t of_n to_n
  end
end
