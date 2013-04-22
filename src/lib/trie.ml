(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

let (!!) = Lazy.force

type ('a,'b) t = {
  value: 'b list;
  children: ('a * ('a,'b) t) list Lazy.t;
}

type ('a, 'b) callback = ('a, 'b) t -> unit

let mk callback children value =
  let tree = { children; value } in
  callback tree;
  tree

let children t =
  !!(t.children)

let create ?(children = lazy []) ?value () =
  let value = match value with Some v -> [v] | None -> [] in
  mk (fun _ -> ()) children value

let empty = create ()

let rec list_map_filter f = function
  | [] -> []
  | h :: tl -> match f h with
    | Some h -> h :: list_map_filter f tl
    | None -> list_map_filter f tl

(* actually a map_filter, which causes it to force all the lazies
   (it's otherwise impossible to know which branches to prune) *)
let map_filter_values callback f tree =
  let rec aux value children =
    let value = list_map_filter f value in
    let children = lazy (
      list_map_filter
        (fun (key, { value; children }) -> match aux value children with
           | { value = []; children = lazy [] } -> None
           | r -> Some (key, r))
        !!children
    ) in
    mk callback children value
  in
  aux tree.value tree.children

let iter f tree =
  let rec aux rev_path tree =
    List.iter (f (List.rev rev_path)) tree.value;
    List.iter (fun (k,v) -> aux (k::rev_path) v) !!(tree.children)
  in
  aux [] tree

let fold0 f tree acc =
  let rec aux acc t rev_path =
    let acc =
      List.fold_left
        (fun acc (key,n) -> aux acc n (key::rev_path))
        acc
        !!(t.children)
    in
    f acc (List.rev rev_path) t.value
  in
  aux acc tree []

let fold f =
  fold0
    (fun acc path values ->
       List.fold_left (fun acc v -> f acc path v) acc values)

let sub tree path =
  let rec aux tree = function
    | [] -> tree
    | h :: tl -> aux (List.assoc h !!(tree.children)) tl
  in
  try aux tree path with Not_found -> empty

let rec find_all tree = function
  | h :: tl -> find_all (List.assoc h !!(tree.children)) tl
  | [] -> tree.value

let find tree path =
  match find_all tree path with
  | v::_ -> v
  | [] -> raise Not_found

let mem tree path =
  let rec aux tree = function
    | h :: tl -> aux (List.assoc h !!(tree.children)) tl
    | [] -> tree.value <> []
  in
  try aux tree path with Not_found -> false

(* maps f on the element of assoc list children with key [key], appending a
   new empty child if necessary *)
let list_map_assoc f children key empty =
  let rec aux acc = function
    | [] -> List.rev_append acc [key, f empty]
    | (k,v) as child :: children ->
        if k = key then
          List.rev_append acc ((key, f v) :: children)
        else
          aux (child::acc) children
  in
  aux [] children

let rec map_subtree callback tree path f = match path with
  | [] -> f tree
  | h :: tl ->
      let children = lazy (
        list_map_assoc (fun n ->map_subtree callback n tl f) !!(tree.children) h empty
      ) in
      mk callback children tree.value

let set callback tree path value =
  map_subtree callback tree path (fun t -> mk callback t.children [value])

let set_lazy callback tree path lazy_value =
  map_subtree callback tree path (fun t -> mk callback t.children [!!lazy_value])

let add callback tree path value =
  map_subtree callback tree path (fun t -> mk callback t.children (value::t.value))

let unset callback tree path =
  map_subtree callback tree path (fun t -> mk callback t.children [])

let rec filter_keys callback f tree =
  let children = lazy (
    list_map_filter
      (fun (key,n) -> if f key then Some (key, filter_keys callback f n) else None)
      !!(tree.children)
  ) in
  mk callback children tree.value

let graft callback tree path node =
  map_subtree callback tree path (fun t -> mk callback node.children t.value)

let graft_lazy callback tree path lazy_node =
  map_subtree callback tree path
    (fun t -> mk callback (lazy !!(!!lazy_node.children)) t.value)

let rec merge callback ?(values = fun v1 v2 -> v2@v1) t1 t2 =
  let rec aux l1 l2 = match l1,l2 with
    | ((k1,v1) as h1 :: tl1), ((k2,v2) as h2 :: tl2) ->
        if k1 < k2 then h1 :: aux tl1 l2 else
        if k2 < k1 then h2 :: aux l1 tl2 else
          (k1, merge callback ~values v1 v2) :: aux tl1 tl2
    | [], l | l, [] -> l
  in
  let value = values t1.value t2.value in
  let compare_keys (k1,_) (k2,_) = compare k1 k2 in
  let children = lazy (
    let c1 = List.sort compare_keys (Lazy.force t1.children) in
    let c2 = List.sort compare_keys (Lazy.force t2.children) in
    aux c1 c2
  ) in
  mk callback children value

let append callback tree (path,node) =
  map_subtree callback tree path (fun t -> merge callback t node)
