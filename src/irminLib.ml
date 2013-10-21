  module Node: sig
    include IrminValue.S
    val create: ?value:key -> (label * key) list -> t
    val children: t -> (label * key) list
    val child: t -> label -> key option
  end
  type t
  val empty: t
  val create: ?value:value -> (label * t) list -> t
  val value: t -> value option
  val mem: t -> label list -> bool
  val find: t -> label list -> value
  val set: t -> label list -> value -> t
  val unset: t -> label list -> t
  val iter: (label list -> value -> unit) -> t -> unit
  val fold: ('acc -> label list -> value -> 'acc) -> t -> 'acc -> 'acc
  val sub: t -> label list -> t
  val graft: t -> label list -> t -> t
  val merge : ?value:(value -> value -> value) -> t -> t -> t
  val append : t -> (label list * t) -> t
  val of_node: (key -> value option) -> (key -> Node.t option) -> Node.t -> t
  val to_node: t -> Node.t list
end


  let (!!) = Lazy.force

  type t =  {
    value   : value Lazy.t option;
    children: (label * t Lazy.t) list;
  }

  let create ?value children =
    let value = match value with
      | None   -> None
      | Some v -> Some (lazy v) in
    let children = List.map (fun (l,t) -> l, lazy t) children in
    { children; value; }

  let value t =
    match t.value with
    | None   -> None
    | Some v -> Some !!v

  let empty =
    create []

  let rec list_map_filter f = function
    | [] -> []
    | h :: tl -> match f h with
      | Some h -> h :: list_map_filter f tl
      | None -> list_map_filter f tl

  let map f tree =
    let rec aux rev_path tree =
      let value = match tree.value with
        | None   -> None
        | Some v -> Some (f (List.rev rev_path) v) in
      let children = List.map
          (fun (key, value) -> key, lazy (aux (key::rev_path) !!value))
          tree.children in
      { value; children }
    in
    aux [] tree

  let iter f tree =
    let rec aux rev_path tree =
      let () = match tree.value with
        | None   -> ()
        | Some v -> f (List.rev rev_path) !!v in
      List.iter (fun (k,v) -> aux (k::rev_path) !!v) tree.children
    in
    aux [] tree

  let fold f tree acc =
    let rec aux acc t rev_path =
      let acc =
        List.fold_right (fun (k,v) acc -> aux acc !!v (k::rev_path)) t.children acc
      in
      match t.value with
      | None   -> acc
      | Some v -> f acc (List.rev rev_path) !!v
    in
    aux acc tree []

  let sub tree path =
    let rec aux tree = function
      | []      -> tree
      | h :: tl -> aux !!(List.assoc h tree.children) tl
    in
    try aux tree path with Not_found -> empty

  let find_lazy tree path =
    let rec aux tree = function
      | h :: tl -> aux !!(List.assoc h tree.children) tl
      | []      -> tree.value
    in
    let res = try aux tree path with Not_found -> None in
    match res with
    | None   -> raise Not_found
    | Some v -> v

  let find tree path =
    !!(find_lazy tree path)

  let mem tree path =
    let rec aux tree = function
      | h :: tl -> aux !!(List.assoc h tree.children) tl
      | [] -> tree.value <> None
    in
    try aux tree path with Not_found -> false

  (* maps f on the element of assoc list children with key [key], appending a
     new empty child if necessary *)
  let list_map_assoc f children key empty =
    let rec aux acc = function
      | []                         -> List.rev_append acc [key, lazy (f empty)]
      | (k,v) as child :: children ->
        if k = key then
          List.rev_append acc ((key, lazy (f !!v)) :: children)
        else
          aux (child::acc) children
    in
    aux [] children

  let rec map_subtree tree path f = match path with
    | [] -> f tree
    | h :: tl ->
      let children =
        list_map_assoc (fun n -> map_subtree n tl f) tree.children h empty
      in
      { tree with children }

  let set_lazy tree path value =
    map_subtree tree path (fun t -> { t with value = Some value })

  let set tree path value =
    set_lazy tree path (lazy value)

  let unset tree path =
    map_subtree tree path (fun t -> { t with value = None })

  let rec filter_keys f tree =
    let children =
      list_map_filter
        (fun (key,n) -> if f key then Some (key, lazy(filter_keys f !!n)) else None)
        tree.children in
    { tree with children }

  let graft tree path node =
    map_subtree tree path (fun t -> { t with children = node.children })

  let rec merge ?(value = V.merge) t1 t2 =
    let rec aux l1 l2 = match l1,l2 with
      | ((k1,v1) as h1 :: tl1), ((k2,v2) as h2 :: tl2) ->
        if k1 < k2 then h1 :: aux tl1 l2 else
        if k2 < k1 then h2 :: aux l1 tl2 else
          (k1, lazy (merge ~value !!v1 !!v2)) :: aux tl1 tl2
      | [], l | l, [] -> l
    in
    let value = match t1.value, t2.value with
      | None  , None   -> None
      | None  , Some v
      | Some v, None   -> Some v
      | Some u, Some v -> Some (lazy (value !!u !!v)) in
    let compare_keys (k1,_) (k2,_) = compare k1 k2 in
    let children =
      let c1 = List.sort compare_keys t1.children in
      let c2 = List.sort compare_keys t2.children in
      aux c1 c2
    in
    {value; children}

  let append tree (path,node) =
    map_subtree tree path (fun t -> merge t node)

  exception Invalid_key of key

  let rec of_node read_value read_node n =
    let read fn k =
      match fn k with
      | None   -> raise (Invalid_key k)
      | Some x -> x
    in
    let value = match n.Node.value with
      | None   -> None
      | Some k -> Some (lazy (read read_value k)) in
    let children = List.map (fun (l, n) ->
        let child = lazy (
          of_node read_value read_node (read read_node n)
        ) in
        l, child
      ) n.Node.children in
    { value; children }

  let to_node t =
    failwith "TODO"
end
