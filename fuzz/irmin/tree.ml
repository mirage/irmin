open Lwt.Infix
open Irmin

module Store =
  Irmin_mem.Make (Metadata.None) (Contents.String) (Path.String_list)
    (Branch.String)
    (Hash.BLAKE2B)

(* Combine [l1] and [l2], dropping elements of the longer one *)
let rec combine_drop l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> []
  | x1 :: rest1, x2 :: rest2 -> (x1, x2) :: combine_drop rest1 rest2

module Generators = struct
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
    fix (fun g ->
        let dir =
          map [ list string_gen; list g ] (fun strs subs ->
              (* To have well-formed trees: *)
              let strs = remove_doublons strs in
              `Tree (combine_drop strs subs))
        in
        choose
          [ map [ string_gen ] (fun str -> `Contents (str, ())); dir; dir; dir ])

  let irmin_tree_gen =
    let open Crowbar in
    map [ concrete_tree_gen ] Store.Tree.of_concrete
end

let create_repo () = Lwt_main.run (Irmin_mem.config () |> Store.Repo.v)

module TreeHash = struct
  let is_contents tree =
    match Store.Tree.destruct tree with `Contents _ -> true | `Node _ -> false

  let make_tree_shallow repo tree =
    let hash = Store.Tree.hash tree in
    let data = if is_contents tree then `Contents (hash, ()) else `Node hash in
    Store.Tree.shallow repo data

  (** Returns [Some] if [tree] could be made partially shallow, [None]
      otherwise. At the top-level, you want to receive [Some]; but internally
      this function handles [None] by keeping some tree non-shallow: that's the
      point, you don't want an entirely shallow tree.

      [randoms] is used as an oracle to make choices. It should not be empty. *)
  let rec make_partially_shallow repo tree randoms =
    match randoms with
    | [] -> Lwt.return_some tree (* do not make shallow *)
    | i :: _ -> (
        if i mod 4 = 0 then
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
              let rec enlarge len l =
                (* Using [l] as a generator, return a list of length >= len *)
                assert (l <> []);
                if List.length l < len then enlarge len (l @ l) else l
              in
              Store.Tree.list tree [] >>= fun dir ->
              let dir_len = List.length dir in
              (* We use [randoms] to shallow differently the siblings within [dir]
                 and we pass a tail of [randoms] in recursive calls, to have
                 variance in subtrees. *)
              let dir' = combine_drop dir (enlarge dir_len randoms) in
              assert (List.length dir' = dir_len);
              Lwt_list.fold_left_s
                (fun (shallowed, acc) ((k, subtree), i) ->
                  (* random oracle to decide whether to make complety shallow
                     (b holds) or partially shallow (b doesn't hold: recurse) *)
                  let b = i mod 2 = 0 in
                  (if b then make_tree_shallow repo subtree |> Lwt.return_some
                  else
                    let sub_randoms =
                      match randoms with
                      | [] -> assert false
                      | [ x ] -> [ x ]
                      | _ :: rest -> rest
                    in
                    make_partially_shallow repo subtree sub_randoms)
                  >>= fun subtree_opt ->
                  let shallowed', subtree' =
                    match subtree_opt with
                    | None -> (shallowed, subtree) (* no change *)
                    | Some subtree' ->
                        (* subtree' is partially shallow *)
                        (true, subtree')
                  in
                  Store.Tree.add_tree acc [ k ] subtree' >>= fun acc ->
                  Lwt.return (shallowed', acc))
                (false, Store.Tree.empty) dir'
              >>= fun (shallowed, tree') ->
              if shallowed then Lwt.return_some tree' else Lwt.return_none)

  (** Test that subtituting subtrees by their [Store.Tree.shallow] version
      doesn't change the tree's top-level hash. *)
  let test_hash_stability repo tree is =
    make_partially_shallow repo tree is >>= fun tree_opt ->
    match tree_opt with
    | None -> Crowbar.bad_test ()
    | Some tree' ->
        let hash_to_string = Irmin.Type.to_string Store.Hash.t in
        let hash_eq = Irmin.Type.(unstage (equal Store.Hash.t)) in
        let hash = Store.Tree.hash tree in
        let hash' = Store.Tree.hash tree' in
        let err_opt =
          if hash_eq hash hash' then None
          else
            Some
              (Printf.sprintf "Hash mismatch: %s <> %s\n" (hash_to_string hash)
                 (hash_to_string hash'))
        in
        (match err_opt with None -> () | Some err -> Crowbar.fail err);
        Lwt.return_unit

  let test_hash_stability r t is = test_hash_stability r t is |> Lwt_main.run
end

let () =
  Crowbar.add_test
    ~name:"Store.Tree.hash t = Store.Tree.hash (make_partially_shallow t)"
    [ Generators.irmin_tree_gen; Crowbar.(list1 int) ]
    (TreeHash.test_hash_stability @@ create_repo ())
