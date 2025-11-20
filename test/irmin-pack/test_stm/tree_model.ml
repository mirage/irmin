module TreeModel = struct
  type t = Contents of string | Node of (string * t) list

  let empty = Node []

  let add tree path contents =
    let rec build_new_subtree path =
      match path with
      | [] -> Contents contents
      | p :: rest -> Node [ (p, build_new_subtree rest) ]
    in

    let rec aux t p =
      match (t, p) with
      | _, [] -> Contents contents
      | Contents _, x :: xs -> Node [ (x, build_new_subtree xs) ]
      | Node existing_paths, x :: xs ->
          if List.mem_assoc x existing_paths then
            let subtree = List.assoc x existing_paths in
            let updated_subtree = aux subtree xs in
            Node
              (List.map
                 (fun (k, v) -> if k = x then (k, updated_subtree) else (k, v))
                 existing_paths)
          else
            let new_subtree = build_new_subtree xs in
            Node ((x, new_subtree) :: existing_paths)
    in
    aux tree path

  let find tree path =
    let rec aux t p =
      match (t, p) with
      | Contents v, [] -> Some v
      | Contents _, _ :: _ -> None
      | Node existing_paths, x :: xs -> (
          match List.assoc_opt x existing_paths with
          | None -> None
          | Some subtree -> aux subtree xs)
      | Node _, [] -> None
    in
    aux tree path

  let rec build_tree path subtree =
    match path with
    | [] -> subtree
    | x :: xs -> Node [ (x, build_tree xs subtree) ]

  let remove tree path =
    let rec aux tree path =
      match (tree, path) with
      | __, [] -> assert false
      | Contents _, _ :: _ -> tree
      | Node subtrees, p :: [] ->
          let nsubtrees = List.filter (fun (name, _) -> name <> p) subtrees in
          Node nsubtrees
      | Node subtrees, p :: ps ->
          let nsubtrees =
            List.map
              (fun (name, subtree) ->
                if name <> p then (name, subtree) else (name, aux subtree ps))
              subtrees
            |> List.filter (fun (_, subtree) ->
                   match subtree with
                   | Contents _ -> true
                   | Node [] -> false
                   | Node _ -> true)
          in
          Node nsubtrees
    in
    match (tree, path) with
    | _, [] -> empty
    | Contents _, _ -> tree
    | Node substrees, _ -> aux (Node substrees) path

  let with_tree tree path (f : t option -> t option) =
    let rec aux t p =
      match (t, p) with
      | _, [] -> assert false
      | Contents _, _ -> (
          match f None with
          | None -> Some t
          | Some subtree -> Some (build_tree path subtree))
      | Node subtrees, x :: [] ->
          let new_subtrees =
            match f (List.assoc_opt x subtrees) with
            | None -> List.filter (fun (k, _) -> k <> x) subtrees
            | Some new_subtree ->
                if new_subtree = Node [] then subtrees
                else (x, new_subtree) :: subtrees
          in
          Some (Node new_subtrees)
      | Node subtrees, x :: xs ->
          let new_subtrees =
            match List.assoc_opt x subtrees with
            | None -> (
                let r = f None in
                match r with
                | None -> subtrees
                | Some new_subtree -> (x, build_tree xs new_subtree) :: subtrees
                )
            | Some _ ->
                List.map
                  (fun (k, subtree) ->
                    if k = x then
                      match aux subtree xs with
                      | None -> (k, subtree)
                      | Some new_subtree -> (k, new_subtree)
                    else (k, subtree))
                  subtrees
          in
          let new_subtrees =
            List.filter
              (fun (_, subtree) ->
                match subtree with Node [] -> false | _ -> true)
              new_subtrees
          in
          Some (Node new_subtrees)
    in
    let r =
      match (tree, path) with
      | Node [], _ -> Some tree
      | _, [] -> f (Some tree)
      | _ -> aux tree path
    in
    match r with None -> tree | Some t -> t

  (* let set_tree path subtree =
    let rec build_tree path subtree =
      match path with
      | [] -> subtree
      | x :: xs -> Node [ (x, build_tree xs subtree) ]
    in
    Root (build_tree path subtree) *)

  (* let to_irmin_tree tree =
    let rec aux t =
      match t with
      | Contents v -> Store.Tree.add (Store.Tree.empty ()) [] v
      | Node subtrees ->
          List.fold_left
            (fun acc (name, subtree) ->
              let stree = aux subtree in
              Store.Tree.add_tree acc [ name ] stree)
            (Store.Tree.empty ()) subtrees
    in
    aux tree *)

  (* let to_string tree =
    let rec aux indent t =
      match t with
      | Contents v -> Printf.sprintf "%sContents (%s)\n" indent v
      | Node subtrees ->
          let subtrees_str =
            List.map
              (fun (name, subtree) ->
                Printf.sprintf "%s%s:\n%s" indent name
                  (aux (indent ^ "  ") subtree))
              subtrees
            |> String.concat ""
          in
          Printf.sprintf "%sNode:\n%s" indent subtrees_str
    in
    aux "" tree *)
end
