include Irmin.Export_for_backends
open Irmin
module Store = Irmin_mem.KV (Contents.String)

module Generators : sig
  val irmin_tree : Store.tree Crowbar.gen
end = struct
  let string_gen =
    let open Crowbar in
    (* Data that is easier to read than random bytes *)
    let strs = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] in
    List.map const strs |> choose

  module StringSet = Set.Make (String)

  let remove_doublons l =
    l |> StringSet.of_list |> StringSet.to_seq |> List.of_seq

  let concrete_tree_gen =
    let open Crowbar in
    fix @@ fun concrete_tree_gen ->
    let dir =
      map [ list string_gen; list concrete_tree_gen ] (fun strs subs ->
          (* To have well-formed trees: *)
          let strs = remove_doublons strs in
          `Tree (List.combine_drop strs subs))
    in
    choose
      [ map [ string_gen ] (fun str -> `Contents (str, ())); dir; dir; dir ]

  let irmin_tree =
    let open Crowbar in
    map [ concrete_tree_gen ] Store.Tree.of_concrete
end

let create_repo () = Lwt_main.run (Irmin_mem.config () |> Store.Repo.v)

module Tree_hash = struct
  let make_tree_shallow repo tree =
    Store.Tree.shallow repo (Store.Tree.kinded_hash tree)

  (** Returns [Some] if [tree] could be made partially shallow, [None]
      otherwise. At the top-level, you want to receive [Some]; but internally
      this function handles [None] by keeping some tree non-shallow: that's the
      point, you don't want an entirely shallow tree.

      [randoms] is used as an oracle to make choices. It should not be empty. *)
  let rec make_partially_shallow repo tree (state : Random.State.t) =
    let i = Random.State.int state 1024 in
    if i mod 2 = 0 then Lwt.return_some tree (* do not make shallow *)
    else if i mod 4 = 0 then
      (* make entirely shallow *)
      Lwt.return_some @@ make_tree_shallow repo tree
    else
      match Store.Tree.destruct tree with
      | `Contents _ ->
          (* maybe make shallow *)
          if i mod 2 = 0 then Lwt.return_none
          else Lwt.return_some @@ make_tree_shallow repo tree
      | `Node _ ->
          (* make partially shallow *)
          let* dir = Store.Tree.list tree [] in
          (* We use [randoms] to shallow differently the siblings within [dir]
             and we pass a tail of [randoms] in recursive calls, to have
             variance in subtrees. *)
          let+ shallowed, tree' =
            Lwt_list.fold_left_s
              (fun (shallowed, acc) (k, subtree) ->
                (* random oracle to decide whether to make complety shallow
                   (b holds) or partially shallow (b doesn't hold: recurse) *)
                let* subtree_opt =
                  if i mod 2 = 0 then
                    make_tree_shallow repo subtree |> Lwt.return_some
                  else make_partially_shallow repo subtree state
                in
                let shallowed', subtree' =
                  match subtree_opt with
                  | None -> (shallowed, subtree) (* no change *)
                  | Some subtree' ->
                      (* subtree' is partially shallow *)
                      (true, subtree')
                in
                let+ acc = Store.Tree.add_tree acc [ k ] subtree' in
                (shallowed', acc))
              (false, Store.Tree.empty)
            @@ dir
          in
          if shallowed then Some tree' else None

  let make_partially_shallow repo tree seed =
    Random.init seed;
    make_partially_shallow repo tree (Random.get_state ())

  let hash_eq = Irmin.Type.(unstage (equal Store.Hash.t))
  let pp_hash = Irmin.Type.pp_dump Store.Hash.t

  (** Test that subtituting subtrees by their [Store.Tree.shallow] version
      doesn't change the tree's top-level hash. *)
  let test_hash_stability repo tree seed =
    make_partially_shallow repo tree seed >|= function
    | None -> Crowbar.bad_test ()
    | Some tree' ->
        let hash, hash' = Store.Tree.(hash tree, hash tree') in
        if not (hash_eq hash hash') then
          Format.kasprintf Crowbar.fail "Hash mismatch: %a <> %a\n" pp_hash hash
            pp_hash hash'

  let test_hash_stability r t is = test_hash_stability r t is |> Lwt_main.run
end

let () =
  Crowbar.add_test
    ~name:"Store.Tree.hash t = Store.Tree.hash (make_partially_shallow t)"
    [ Generators.irmin_tree; Crowbar.int ]
    (Tree_hash.test_hash_stability @@ create_repo ())
