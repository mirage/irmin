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

  let close c =
    Store.Private.Repo.clear c.index.repo >>= fun () ->
    Store.Repo.close c.index.repo

  let gc index commit =
    Store.freeze index.repo ~max:[ commit ] >>= fun () ->
    let tree = Store.Tree.empty in
    let ctxt = { index; tree; parents = [] } in
    Lwt.return ctxt

  let create_block1 ctxt =
    set ctxt [ "a"; "b" ] "Novembre" >>= fun ctxt ->
    set ctxt [ "a"; "c" ] "Juin" >>= fun ctxt ->
    set ctxt [ "version" ] "0.0" >>= fun ctxt ->
    commit ctxt >|= fun h -> (ctxt, h)

  let create_block1a ctxt =
    del ctxt [ "a"; "b" ] >>= fun ctxt ->
    set ctxt [ "a"; "d" ] "Mars" >>= fun ctxt ->
    commit ctxt >|= fun h -> (ctxt, h)

  let create_block1b ctxt =
    del ctxt [ "a"; "c" ] >>= fun ctxt ->
    set ctxt [ "a"; "d" ] "Février" >>= fun ctxt ->
    commit ctxt >|= fun h -> (ctxt, h)

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
          loop (i + 1) (([ "a"; k1; k2 ], v) :: aux)
      in
      loop 0 []
    in
    Lwt_list.fold_left_s (fun ctxt (k, v) -> set ctxt k v) ctxt large_list
    >>= fun ctxt ->
    commit ctxt >|= fun h -> (ctxt, h)

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
        create_large_tree ctxt (k, v) >>= fun (ctxt, block) ->
        checkout ctxt.index block >>= function
        | None -> failwith "checkout block"
        | Some ctxt -> Lwt.return (ctxt, (k, v, block) :: commits))
      (ctxt, []) small_list

  let test_simple_gc () =
    let store_name = fresh_name () in
    init store_name >>= fun ctxt ->
    create_block1 ctxt >>= fun (ctxt, block1) ->
    checkout_and_create ctxt.index block1 create_block1a
    >>= fun (ctxt, block1a) ->
    checkout_and_create ctxt.index block1 create_block1b
    >>= fun (ctxt, block1b) ->
    gc ctxt.index block1a >>= fun ctxt ->
    checkout ctxt.index block1 >>= function
    | None -> Alcotest.fail "checkout block1"
    | Some ctxt -> (
        get ctxt [ "version" ] >>= fun version ->
        Alcotest.(check (option string)) "version.1" version (Some "0.0");
        get ctxt [ "a"; "b" ] >>= fun novembre ->
        Alcotest.(check (option string)) "nov" (Some "Novembre") novembre;
        get ctxt [ "a"; "c" ] >>= fun juin ->
        Alcotest.(check (option string)) "juin" (Some "Juin") juin;
        ( if not (Store.async_freeze ()) then
          checkout ctxt.index block1b >>= function
          | None -> Lwt.return_unit
          | Some _ -> Alcotest.fail "should not find block1b"
        else Lwt.return_unit )
        >>= fun () ->
        checkout ctxt.index block1a >>= function
        | None -> Alcotest.fail "checkout block1a"
        | Some ctxt ->
            get ctxt [ "version" ] >>= fun version ->
            Alcotest.(check (option string)) "version.2" version (Some "0.0");
            get ctxt [ "a"; "d" ] >>= fun mars ->
            Alcotest.(check (option string)) "mars" (Some "Mars") mars;
            get ctxt [ "a"; "c" ] >>= fun juin ->
            Alcotest.(check (option string)) "juin" (Some "Juin") juin;
            Lwt.return_unit >>= fun () -> close ctxt )

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
    init store_name >>= fun ctxt ->
    commit_large_trees ctxt >>= fun (ctxt, blocks) ->
    let _, _, block = List.hd blocks in
    let create1 ctxt =
      set ctxt [ "a"; "d" ] "Mars" >>= fun ctxt ->
      commit ctxt >|= fun h -> (ctxt, h)
    in
    let create2 ctxt =
      set ctxt [ "a"; "d" ] "Février" >>= fun ctxt ->
      commit ctxt >|= fun h -> (ctxt, h)
    in
    let create3 ctxt =
      set ctxt [ "a"; "b" ] "Février" >>= fun ctxt ->
      commit ctxt >|= fun h -> (ctxt, h)
    in
    checkout_and_create ctxt.index block create1 >>= fun (ctxt, block1a) ->
    checkout_and_create ctxt.index block create2 >>= fun (ctxt, block1b) ->
    gc ctxt.index block1a >>= fun ctxt ->
    checkout_and_create ctxt.index block create3 >>= fun (ctxt, block1c) ->
    checkout ctxt.index block1a >>= function
    | None -> Alcotest.fail "checkout block1a"
    | Some ctxt -> (
        get ctxt [ "a"; "d" ] >>= fun mars ->
        Alcotest.(check (option string)) "mars" (Some "Mars") mars;
        checkout ctxt.index block1c >>= function
        | None -> Alcotest.fail "checkout block1c"
        | Some ctxt ->
            get ctxt [ "a"; "b" ] >>= fun fevr ->
            Alcotest.(check (option string)) "fevrier" (Some "Février") fevr;
            ( if not (Store.async_freeze ()) then
              checkout ctxt.index block1b >|= function
              | None -> ()
              | Some _ -> Alcotest.fail "should not find block1b"
            else Lwt.return_unit )
            >>= fun () ->
            Lwt_list.iter_s
              (fun (k, v, block) -> check_large_blocks ctxt.index block k v)
              blocks
            >>= fun () -> close ctxt )

  let test_open_lower () =
    let store_name = fresh_name () in
    init store_name >>= fun ctxt ->
    StoreSimple.Repo.v
      (config ~fresh:false ~readonly:true
         (Filename.concat store_name Conf.lower_root))
    >>= fun repo ->
    create_block1 ctxt >>= fun (ctxt, block1) ->
    gc ctxt.index block1 >>= fun ctxt ->
    Store.PrivateLayer.wait_for_freeze () >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    let hash1 = Store.Commit.hash block1 in
    (StoreSimple.Commit.of_hash repo hash1 >>= function
     | None -> Alcotest.fail "checkout block1"
     | Some commit ->
         let tree = StoreSimple.Commit.tree commit in
         StoreSimple.Tree.find tree [ "version" ] >>= fun version ->
         Alcotest.(check (option string)) "version" version (Some "0.0");
         Lwt.return_unit)
    >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () -> StoreSimple.Repo.close repo

  let test_two_rw_instances () =
    let store_name = fresh_name () in
    init store_name >>= fun ctxt ->
    Store.Repo.v (config ~readonly:false ~fresh:false store_name)
    >>= fun repo ->
    create_block1 ctxt >>= fun (ctxt, block1) ->
    gc ctxt.index block1 >>= fun ctxt ->
    (Store.Commit.of_hash repo (Store.Commit.hash block1) >>= function
     | None -> Alcotest.fail "no hash found in repo"
     | Some commit ->
         let tree = Store.Commit.tree commit in
         Store.Tree.find tree [ "a"; "b" ] >|= fun novembre ->
         Alcotest.(check (option string)) "nov" (Some "Novembre") novembre)
    >>= fun () ->
    Store.PrivateLayer.wait_for_freeze () >>= fun () ->
    (Store.Commit.of_hash repo (Store.Commit.hash block1) >>= function
     | None -> Alcotest.fail "no hash found in repo"
     | Some commit ->
         let tree = Store.Commit.tree commit in
         Store.Tree.find tree [ "a"; "b" ] >|= fun novembre ->
         Alcotest.(check (option string)) "nov" (Some "Novembre") novembre)
    >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () -> Store.Repo.close repo

  let test_rw_ro_instances () =
    let store_name = fresh_name () in
    init store_name >>= fun ctxt ->
    Store.Repo.v (config ~readonly:true ~fresh:false store_name) >>= fun repo ->
    create_block1 ctxt >>= fun (ctxt, block1) ->
    let check_block =
      Store.Commit.of_hash repo (Store.Commit.hash block1) >>= function
      | None -> Alcotest.fail "no hash found in repo"
      | Some commit ->
          let tree = Store.Commit.tree commit in
          Store.Tree.find tree [ "a"; "b" ] >|= fun novembre ->
          Alcotest.(check (option string)) "nov" (Some "Novembre") novembre
    in
    check_block >>= fun () ->
    gc ctxt.index block1 >>= fun ctxt ->
    check_block >>= fun () ->
    Store.PrivateLayer.wait_for_freeze () >>= fun () ->
    check_block >>= fun () ->
    checkout_and_create ctxt.index block1 create_block1a
    >>= fun (ctxt, block1a) ->
    let check_block1a =
      Store.Commit.of_hash repo (Store.Commit.hash block1a) >>= function
      | None -> Alcotest.fail "no hash found in repo"
      | Some commit ->
          let tree = Store.Commit.tree commit in
          Store.Tree.find tree [ "a"; "d" ] >|= fun mars ->
          Alcotest.(check (option string)) "mars" (Some "Mars") mars
    in
    check_block1a >>= fun () ->
    gc ctxt.index block1a >>= fun ctxt ->
    checkout_and_create ctxt.index block1 create_block1b
    >>= fun (ctxt, block1b) ->
    (Store.Commit.of_hash repo (Store.Commit.hash block1b) >>= function
     | None -> Alcotest.fail "no hash found in repo"
     | Some commit ->
         let tree = Store.Commit.tree commit in
         Store.Tree.find tree [ "a"; "d" ] >|= fun fevrier ->
         Alcotest.(check (option string)) "fevrier" (Some "Février") fevrier)
    >>= fun () ->
    check_block1a >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () -> Store.Repo.close repo

  let _test_close_and_reopen () =
    let store_name = fresh_name () in
    init store_name >>= fun ctxt ->
    create_block1 ctxt >>= fun (ctxt, block1) ->
    gc ctxt.index block1 >>= fun ctxt ->
    checkout_and_create ctxt.index block1 create_block1a
    >>= fun (ctxt, block1a) ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    Store.Repo.v (config ~readonly:false ~fresh:false store_name)
    >>= fun repo ->
    (Store.Commit.of_hash repo (Store.Commit.hash block1) >>= function
     | None -> Alcotest.fail "no hash found in repo"
     | Some commit ->
         let tree = Store.Commit.tree commit in
         Store.Tree.find tree [ "a"; "b" ] >|= fun novembre ->
         Alcotest.(check (option string)) "nov" (Some "Novembre") novembre)
    >>= fun () ->
    Store.Commit.of_hash repo (Store.Commit.hash block1a) >>= function
    | None -> Alcotest.fail "no hash found in repo"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        Store.Tree.find tree [ "a"; "d" ] >|= fun mars ->
        Alcotest.(check (option string)) "mars" (Some "Mars") mars

  let tests =
    [
      Alcotest.test_case "Test simple freeze" `Quick (fun () ->
          Lwt_main.run (test_simple_gc ()));
      Alcotest.test_case "Tests large commits" `Quick (fun () ->
          Lwt_main.run (test_large_commit ()));
      Alcotest.test_case "Test open lower" `Quick (fun () ->
          Lwt_main.run (test_open_lower ()));
      Alcotest.test_case "Test two rw instances" `Quick (fun () ->
          Lwt_main.run (test_two_rw_instances ()));
      Alcotest.test_case "Test rw and ro instances" `Quick (fun () ->
          Lwt_main.run (test_rw_ro_instances ()));
      (*Alcotest.test_case "Test close and reopen a store" `Quick (fun () ->
          Lwt_main.run (test_close_and_reopen ()));*)
    ]
end

let tests = Tezos_Usecase.tests
