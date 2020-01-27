open Lwt.Infix

(* this is based on a simplified version of the tezos simple_test *)

let root = "_build"

let fresh_name =
  let c = ref 0 in
  fun () ->
    incr c;
    let name = Filename.concat root ("layered_" ^ string_of_int !c) in
    Logs.info (fun m -> m "Constructing irmin store %s" name);
    name

let index_log_size = Some 1_000

module Conf = struct
  let entries = 32

  let stable_hash = 256

  let lower_root = "_lower"
end

module Hash = Irmin.Hash.SHA1
module Store =
  Irmin_pack.Make_layered (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)
module StoreSimple =
  Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

let config ?(readonly = false) ?(fresh = true) ?(lower_root = Conf.lower_root)
    root =
  let conf = Irmin_pack.config ~readonly ?index_log_size ~fresh root in
  Irmin_layers.config ~conf ~lower_root root

let random_char () = char_of_int (33 + Random.int 94)

let random_string string_size =
  String.init string_size (fun _i -> random_char ())

module Tezos_Usecase = struct
  type index = { path : string; repo : Store.Repo.t }

  and context = {
    index : index;
    parents : Store.Commit.t list;
    tree : Store.tree;
  }

  let info = Irmin.Info.v ~date:0L ~author:"" ""

  let commit context =
    let parents = List.map Store.Commit.hash context.parents in
    Store.Commit.v context.index.repo ~info ~parents context.tree >|= fun h ->
    Store.Tree.clear context.tree;
    h

  let set ctxt key data =
    Store.Tree.add ctxt.tree key data >>= fun tree ->
    Lwt.return { ctxt with tree }

  let get ctxt key = Store.Tree.find ctxt.tree key

  let del ctxt key =
    Store.Tree.remove ctxt.tree key >>= fun tree ->
    Lwt.return { ctxt with tree }

  let checkout index key =
    Store.Commit.of_hash index.repo (Store.Commit.hash key) >>= function
    | None -> Lwt.return_none
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let ctxt = { index; tree; parents = [ commit ] } in
        Lwt.return_some ctxt

  let init ?readonly root =
    Store.Repo.v (config ?readonly root) >>= fun repo ->
    let index = { path = root; repo } in
    let tree = Store.Tree.empty in
    let ctxt = { index; tree; parents = [] } in
    Lwt.return ctxt

  let gc index commit =
    Store.freeze index.repo ~max:[ commit ] >>= fun _ ->
    let tree = Store.Tree.empty in
    let ctxt = { index; tree; parents = [] } in
    Lwt.return ctxt

  let create_block1 ctxt =
    set ctxt [ "a"; "b" ] "Novembre" >>= fun ctxt ->
    set ctxt [ "a"; "c" ] "Juin" >>= fun ctxt ->
    set ctxt [ "version" ] "0.0" >>= fun ctxt -> commit ctxt

  let create_block1a ctxt =
    del ctxt [ "a"; "b" ] >>= fun ctxt ->
    set ctxt [ "a"; "d" ] "Mars" >>= fun ctxt -> commit ctxt

  let create_block1b ctxt =
    del ctxt [ "a"; "c" ] >>= fun ctxt ->
    set ctxt [ "a"; "d" ] "Février" >>= fun ctxt -> commit ctxt

  let checkout_and_create idx block create =
    checkout idx block >>= function
    | None -> failwith "checkout block"
    | Some ctxt -> create ctxt

  let create_large_tree ctxt first =
    let large_list =
      let rec loop i aux =
        if i = 50 then first :: aux
        else
          let k1 = random_string 5 in
          let k2 = random_string 5 in
          let v = random_string 5 in
          loop (i + 1) (([ k1; k2 ], v) :: aux)
      in
      loop 0 []
    in
    Lwt_list.fold_left_s (fun ctxt (k, v) -> set ctxt k v) ctxt large_list
    >>= fun ctxt -> commit ctxt

  let commit_large_trees ctxt =
    let small_list =
      let rec loop i aux =
        if i = 10 then aux
        else
          let k1 = random_string 2 in
          let k2 = random_string 2 in
          let v = random_string 1 in
          loop (i + 1) (([ k1; k2 ], v) :: aux)
      in
      loop 0 []
    in
    Lwt_list.fold_left_s
      (fun (ctxt, commits) (k, v) ->
        create_large_tree ctxt (k, v) >>= fun block ->
        checkout ctxt.index block >>= function
        | None -> failwith "checkout block"
        | Some ctxt -> Lwt.return (ctxt, (k, v, block) :: commits))
      (ctxt, []) small_list

  let test_simple_gc () =
    let store_name = fresh_name () in
    init (Filename.concat store_name "A") >>= fun c ->
    create_block1 c >>= fun block1 ->
    checkout_and_create c.index block1 create_block1a >>= fun block1a ->
    checkout_and_create c.index block1 create_block1b >>= fun block1b ->
    gc c.index block1a >>= fun c ->
    checkout c.index block1 >>= function
    | None -> Alcotest.fail "checkout block1"
    | Some ctxt -> (
        ( get ctxt [ "version" ] >>= fun version ->
          Alcotest.(check (option string)) "version.1" version (Some "0.0");
          get ctxt [ "a"; "b" ] >>= fun novembre ->
          Alcotest.(check (option string)) "nov" (Some "Novembre") novembre;
          get ctxt [ "a"; "c" ] >>= fun juin ->
          Alcotest.(check (option string)) "juin" (Some "Juin") juin;
          Lwt.return_unit )
        >>= fun () ->
        (checkout c.index block1b >>= function
         | None -> Lwt.return_unit
         | Some _ -> Alcotest.fail "should not find block1b")
        >>= fun () ->
        checkout c.index block1a >>= function
        | None -> Alcotest.fail "checkout block1a"
        | Some ctxt ->
            get ctxt [ "version" ] >>= fun version ->
            Alcotest.(check (option string)) "version.2" version (Some "0.0");
            get ctxt [ "a"; "d" ] >>= fun mars ->
            Alcotest.(check (option string)) "mars" (Some "Mars") mars;
            get ctxt [ "a"; "c" ] >>= fun juin ->
            Alcotest.(check (option string)) "juin" (Some "Juin") juin;
            Lwt.return_unit )

  let test_large_commit () =
    let check_large_blocks index block k v =
      checkout index block >>= function
      | None -> Alcotest.fail "checkout block"
      | Some ctxt ->
          get ctxt k >>= fun v' ->
          Alcotest.(check (option string)) "first commit" v' (Some v);
          Lwt.return_unit
    in
    let store_name = fresh_name () in
    init (Filename.concat store_name "A") >>= fun c ->
    commit_large_trees c >>= fun (c, blocks) ->
    Printf.printf "commited many large lists\n %!";
    let _, _, block = List.hd blocks in
    let create1 ctxt =
      set ctxt [ "a"; "d" ] "Mars" >>= fun ctxt -> commit ctxt
    in
    let create2 ctxt =
      set ctxt [ "a"; "d" ] "Février" >>= fun ctxt -> commit ctxt
    in
    checkout_and_create c.index block create1 >>= fun block1a ->
    checkout_and_create c.index block create2 >>= fun block1b ->
    Printf.printf "gc - keep only block1a \n %!";
    gc c.index block1a >>= fun c ->
    Printf.printf "after gc \n %!";
    checkout c.index block1a >>= function
    | None -> Alcotest.fail "checkout block11"
    | Some ctxt -> (
        Printf.printf "got ctxt on large list\n %!";
        get ctxt [ "a"; "d" ] >>= fun mars ->
        Alcotest.(check (option string)) "mars" (Some "Mars") mars;
        Lwt.return_unit >>= fun () ->
        (checkout c.index block1b >>= function
         | None -> Lwt.return_unit
         | Some _ -> Alcotest.fail "should not find block1b")
        >>= function
        | () ->
            Lwt_list.iter_s
              (fun (k, v, block) -> check_large_blocks c.index block k v)
              blocks )

  let test_open_lower () =
    let store_name = fresh_name () in
    init store_name >>= fun c ->
    create_block1 c >>= fun block1 ->
    gc c.index block1 >>= fun c2 ->
    StoreSimple.Repo.v
      (config ~fresh:false (Filename.concat store_name Conf.lower_root))
    >>= fun c3 ->
    StoreSimple.Commit.of_hash c3 (Store.Commit.hash block1) >>= function
    | None -> Alcotest.fail "checkout block1"
    | Some commit ->
        let tree = StoreSimple.Commit.tree commit in
        StoreSimple.Tree.find tree [ "version" ] >>= fun version ->
        Alcotest.(check (option string)) "version" version (Some "0.0");
        Lwt.return_unit >>= fun () ->
        Store.Repo.close c2.index.repo >>= fun () -> StoreSimple.Repo.close c3

  let tests =
    [
      Alcotest.test_case "Test simple freeze" `Quick (fun () ->
          Lwt_main.run (test_simple_gc ()));
      Alcotest.test_case "Tests large commits" `Quick (fun () ->
          Lwt_main.run (test_large_commit ()));
      Alcotest.test_case "Test open lower" `Quick (fun () ->
          Lwt_main.run (test_open_lower ()));
    ]
end

let tests = Tezos_Usecase.tests
