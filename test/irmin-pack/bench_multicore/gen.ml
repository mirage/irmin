type range = int * int

type config = {
  elements : int;
  branching : range;
  balance : range;
  name_length : range;
  contents_length : range;
  max_depth : int;
  nb_tasks : int;
  nb_finds : int;
  nb_adds : int;
  nb_rems : int;
  nb_runs : int;
  warm : bool;
}

type path = string list
type op = Find of path | Add of path * bytes | Rem of path

let random_range (min, max) =
  if min >= max then min else min + Random.int (max - min)

let random_string len =
  String.init len (fun _ -> Char.chr (Char.code 'a' + Random.int 26))

let make_name config =
  let len = random_range config.name_length in
  random_string len

let make_contents config =
  let len = random_range config.contents_length in
  Bytes.of_string @@ random_string len

let make_balanced config quantity' =
  assert (quantity' > 1);
  let len = random_range config.branching in
  let quantity = quantity' - len in
  if quantity <= 0 then List.init (quantity' - 1) (fun _ -> 1)
  else
    let weights = List.init len (fun _ -> random_range config.balance) in
    let total = List.fold_left ( + ) 0 weights in
    let weights = List.map (fun w -> 1 + (quantity * w / total)) weights in
    let total = List.fold_left ( + ) 0 weights in
    let rest = quantity' - total in
    assert (rest >= 0);
    let out = match weights with [] -> [] | hd :: tl -> (hd + rest) :: tl in
    let retotal = List.fold_left ( + ) 0 out in
    assert (retotal = quantity');
    out

let rec make_paths ~config ~depth ~quantity ~path acc =
  assert (quantity > 0);
  if quantity = 1 then (path, make_contents config) :: acc
  else if depth = 0 then
    let children =
      List.init quantity (fun _ ->
          let name = make_name config in
          let path = name :: path in
          (path, make_contents config))
    in
    List.rev_append children acc
  else
    let depth = depth - 1 in
    List.fold_left
      (fun acc quantity ->
        let name = make_name config in
        let path = name :: path in
        make_paths ~config ~depth ~quantity ~path acc)
      acc
      (make_balanced config quantity)

let array_shuffle arr =
  for i = 0 to Array.length arr - 1 do
    let j = Random.int (Array.length arr - i) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done

let make_paths ?(path = []) ~config () =
  let q0 = config.elements in
  let q1 = if config.nb_adds <= 0 then q0 else 2 * q0 in
  let all = make_paths ~config ~depth:config.max_depth ~quantity:q1 ~path [] in
  let all = Array.of_list all in
  let all = Array.map (fun (path, contents) -> (List.rev path, contents)) all in
  array_shuffle all;
  let init = min (Array.length all) q0 in
  (Array.sub all 0 init, Array.sub all init (Array.length all - init))

let random_sample nb arr =
  Array.init nb (fun _ -> arr.(Random.int (Array.length arr)))

let make_task ~config paths add_paths =
  let to_find =
    Array.map (fun (name, _) -> Find name)
    @@ random_sample config.nb_finds paths
  in
  let to_rem =
    Array.map (fun (name, _) -> Rem name) @@ random_sample config.nb_rems paths
  in
  let to_add =
    Array.map (fun (name, contents) -> Add (name, contents))
    @@ random_sample config.nb_adds add_paths
  in
  let task = Array.concat [ to_find; to_add; to_rem ] in
  array_shuffle task;
  task

let make ~config =
  let paths, add_paths = make_paths ~config () in
  let tasks =
    Array.init config.nb_tasks (fun _ -> make_task ~config paths add_paths)
  in
  (paths, tasks)

let make_full ~config =
  let subtree_config =
    { config with elements = config.elements / config.nb_tasks; nb_tasks = 1 }
  in
  let sub =
    List.init config.nb_tasks (fun i ->
        let i = string_of_int i in
        let paths, add_paths =
          make_paths ~path:[ i ] ~config:subtree_config ()
        in
        let task = make_task ~config:subtree_config paths add_paths in
        (paths, task))
  in
  let paths = Array.concat @@ List.map fst sub in
  let tasks = Array.of_list @@ List.map snd sub in
  (paths, tasks)
