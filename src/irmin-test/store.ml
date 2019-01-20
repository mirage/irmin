(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Common

let src = Logs.Src.create "test" ~doc:"Irmin tests"
module Log = (val Logs.src_log src : Logs.LOG)

module T = Irmin.Type

let merge_exn msg x = match x with
  | Ok x                -> Lwt.return x
  | Error (`Conflict m) -> Alcotest.failf "%s: %s" msg m

let info msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = Printf.sprintf "TESTS" in
  Irmin.Info.v ~date ~author msg

let infof fmt = Fmt.kstrf (fun str () -> info str) fmt

let () = Random.self_init ()
let random_char () = char_of_int (Random.int 256)
let random_ascii () =
  let chars = "0123456789abcdefghijklmnopqrstABCDEFGHIJKLMNOPQRST-_." in
  String.get chars (Random.int @@ String.length chars)

let random_string n = String.init n (fun _i -> random_char ())
let long_random_string = random_string (* 1024_000 *) 10

let random_ascii_string n = String.init n (fun _i -> random_ascii ())
let long_random_ascii_string = random_ascii_string 1024_000

module Make (S: S) = struct

  module P = S.Private
  module Graph = Irmin.Private.Node.Graph(P.Node)
  module History = Irmin.Private.Commit.History(P.Commit)

  let v repo = P.Repo.contents_t repo
  let n repo = P.Repo.node_t repo
  let ct repo = P.Repo.commit_t repo
  let g repo = P.Repo.node_t repo
  let h repo = P.Repo.commit_t repo

  let v1 = long_random_string
  let v2 = ""

  let with_contents repo f = P.Repo.batch repo (fun t _ _ -> f t)
  let with_node repo f = P.Repo.batch repo (fun _ t _ -> f t)
  let with_commit repo f = P.Repo.batch repo (fun _ _ t -> f t)

  let kv1 ~repo = with_contents repo (fun t -> P.Contents.add t v1)
  let kv2 ~repo = with_contents repo (fun t -> P.Contents.add t v2)
  let normal x = `Contents (x, S.Metadata.default)

  let b1 = "foo"
  let b2 = "bar/toto"

  let n1 ~repo =
    kv1 ~repo >>= fun kv1 ->
    with_node repo (fun t -> Graph.v t ["x", normal kv1])

  let n2 ~repo =
    n1 ~repo >>= fun kn1 ->
    with_node repo (fun t -> Graph.v t ["b", `Node kn1])

  let n3 ~repo =
    n2 ~repo >>= fun kn2 ->
    with_node repo (fun t -> Graph.v t ["a", `Node kn2])

  let n4 ~repo =
    n1 ~repo >>= fun kn1 ->
    kv2 ~repo >>= fun kv2 ->
    with_node repo (fun t -> Graph.v t ["x", normal kv2])
    >>= fun kn4 ->
    with_node repo (fun t -> Graph.v t ["b", `Node kn1; "c", `Node kn4])
    >>= fun kn5 ->
    with_node repo (fun t -> Graph.v t ["a", `Node kn5])

  let r1 ~repo =
    n2 ~repo >>= fun kn2 ->
    S.Tree.of_hash repo kn2 >>= function
    | None      -> Alcotest.fail "r1"
    | Some tree ->
      S.Commit.v repo ~info:Irmin.Info.empty ~parents:[] (tree :> S.tree)

  let r2 ~repo =
    n3 ~repo >>= fun kn3 ->
    r1 ~repo >>= fun kr1 ->
    S.Tree.of_hash repo kn3 >>= function
    | None    -> Alcotest.fail "r2"
    | Some t3 ->
      S.Commit.v repo ~info:Irmin.Info.empty  ~parents:[kr1] (t3 :> S.tree)

  let run x test =
    try
      Lwt_main.run (
        x.init () >>= fun () ->
        S.Repo.v x.config >>= fun repo ->
        test repo >>= x.clean
      )
    with e ->
      Lwt_main.run (x.clean ());
      raise e

  let random_value value = random_string value

  let random_path ~label ~path =
    let short () = random_ascii_string label in
    let rec aux = function
      | 0 -> []
      | n -> short () :: aux (n-1)
    in
    aux path

  let random_node ~label ~path ~value =
    random_path ~label ~path, random_value value

  let random_nodes ?(label=8) ?(path=5) ?(value=1024) n =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (random_node ~label ~path ~value :: acc) (n-1) in
    aux [] n

  let sleep ?(sleep_t=0.01) () =
    let sleep_t = min sleep_t 1. in
    Lwt_unix.yield () >>= fun () ->
    Lwt_unix.sleep sleep_t

  let retry ?(timeout=15.) ?(sleep_t=0.) fn =
    let sleep_t = max sleep_t 0.001 in
    let time = Unix.gettimeofday in
    let t = time () in
    let str i = Fmt.strf "%d, %.3fs" i (time () -. t) in
    let rec aux i =
      if time () -. t > timeout then fn (str i);
      try fn (str i); Lwt.return_unit
      with ex ->
        Log.debug (fun f -> f "retry ex: %s" (Printexc.to_string ex));
        let sleep_t = sleep_t *. (1. +. float i ** 2.) in
        sleep ~sleep_t () >>= fun () ->
        Log.debug (fun f -> f "Test.retry %s" (str i));
        aux (i+1)
    in
    aux 0

  let old k () = Lwt.return (Ok (Some k))

  let test_contents x () =
    let test repo =
      let t = P.Repo.contents_t repo in
      let check_key = check P.Contents.Key.t in
      let check_val = check (T.option S.contents_t) in
      kv2 ~repo >>= fun kv2 ->
      with_contents repo (fun t -> P.Contents.add t v2) >>= fun k2' ->
      check_key "kv2" kv2 k2';
      P.Contents.find t k2' >>= fun v2' ->
      check_val "v2" (Some v2) v2';

      with_contents repo (fun t -> P.Contents.add t v2) >>= fun k2'' ->
      check_key "kv2" kv2 k2'';

      kv1 ~repo >>= fun kv1 ->
      with_contents repo (fun t -> P.Contents.add t v1) >>= fun k1' ->
      check_key "kv1" kv1 k1';
      with_contents repo (fun t -> P.Contents.add t v1) >>= fun k1'' ->
      check_key "kv1" kv1 k1'';
      P.Contents.find t kv1 >>= fun v1' ->
      check_val "v1" (Some v1) v1';
      P.Contents.find t kv2 >>= fun v2' ->
      check_val "v2" (Some v2) v2';
      Lwt.return_unit
    in
    run x test

  let get = function None -> Alcotest.fail "get" | Some v -> v
  let get_node = function `Node n -> n | _ -> Alcotest.fail "get_node"

  let test_nodes x () =
    let test repo =
      let g = g repo and n = n repo in
      kv1 ~repo >>= fun kv1 ->
      let check_key = check P.Node.Key.t in
      let check_val = check (T.option Graph.value_t) in

      (* Create a node containing t1 -x-> (v1) *)
      with_node repo (fun g -> Graph.v g ["x", normal kv1]) >>= fun k1 ->
      with_node repo (fun g -> Graph.v g ["x", normal kv1]) >>= fun k1' ->
      check_key "k1.1" k1 k1';
      P.Node.find n k1 >>= fun t1 ->
      with_node repo (fun n -> P.Node.add n (get t1)) >>= fun k1''->
      check_key "k1.2" k1 k1'';

      (* Create the node  t2 -b-> t1 -x-> (v1) *)
      with_node repo (fun g -> Graph.v g ["b", `Node k1]) >>= fun k2 ->
      with_node repo (fun g -> Graph.v g ["b", `Node k1]) >>= fun k2' ->
      check_key "k2.1" k2 k2';
      P.Node.find n k2 >>= fun t2 ->
      with_node repo (fun n -> P.Node.add n (get t2)) >>= fun k2''->
      check_key "k2.2" k2 k2'';
      Graph.find g k2 ["b"] >>= fun k1''' ->
      check_val "k1.3" (Some (`Node k1)) k1''';

      (* Create the node t3 -a-> t2 -b-> t1 -x-> (v1) *)
      with_node repo (fun g -> Graph.v g ["a", `Node k2]) >>= fun k3 ->
      with_node repo (fun g -> Graph.v g ["a", `Node k2]) >>= fun k3' ->
      check_key "k3.1" k3 k3';
      P.Node.find n k3 >>= fun t3 ->
      with_node repo (fun n -> P.Node.add n (get t3)) >>= fun k3''->
      check_key "k3.2" k3 k3'';
      Graph.find g k3 ["a"] >>= fun k2'' ->
      check_val "k2.3" (Some (`Node k2)) k2'';

      Graph.find g k2' ["b"] >>= fun k1'''' ->
      check_val "t1.2" (Some (`Node k1)) k1'''';
      Graph.find g k3 ["a";"b"] >>= fun k1'''''->
      check_val "t1.3" (Some (`Node k1)) k1''''';

      Graph.find g k1 ["x"] >>= fun kv11 ->
      check_val "v1.1" (Some (normal kv1)) kv11;
      Graph.find g k2 ["b";"x"] >>= fun kv12 ->
      check_val "v1.2" (Some (normal kv1)) kv12;
      Graph.find g k3 ["a";"b";"x"] >>= fun kv13 ->
      check_val "v1" (Some (normal kv1)) kv13;

      (* Create the node t6 -a-> t5 -b-> t1 -x-> (v1)
                                   \-c-> t4 -x-> (v2) *)
      kv2 ~repo >>= fun kv2 ->
      with_node repo (fun g -> Graph.v g ["x", normal kv2]) >>= fun k4 ->
      with_node repo (fun g -> Graph.v g ["b", `Node k1; "c", `Node k4])
      >>= fun k5 ->
      with_node repo (fun g -> Graph.v g ["a", `Node k5]) >>= fun k6 ->
      with_node repo (fun g -> Graph.update g k3 ["a";"c";"x"] (normal kv2))
      >>= fun k6' ->
      P.Node.find n k6' >>= fun n6' ->
      P.Node.find n k6  >>= fun n6 ->
      check T.(option P.Node.Val.t) "node n6" n6 n6';
      check_key "node k6" k6 k6';

      let assert_no_duplicates n node =
        let names = ref [] in
        Graph.list g node >|= fun all ->
        List.iter (fun (s, _) ->
            if List.mem s !names then Alcotest.failf "%s: duplicate!" n
            else names := s :: !names
          ) all
      in
      with_node repo (fun g -> Graph.v g []) >>= fun n0 ->

      with_node repo (fun g -> Graph.update g n0 ["b"] (`Node n0)) >>= fun n1 ->
      with_node repo (fun g -> Graph.update g n1 ["a"] (`Node n0)) >>= fun n2 ->
      with_node repo (fun g -> Graph.update g n2 ["a"] (`Node n0)) >>= fun n3 ->
      assert_no_duplicates "1" n3 >>= fun () ->

      with_node repo (fun g -> Graph.update g n0 ["a"] (`Node n0)) >>= fun n1 ->
      with_node repo (fun g -> Graph.update g n1 ["b"] (`Node n0)) >>= fun n2 ->
      with_node repo (fun g -> Graph.update g n2 ["a"] (`Node n0)) >>= fun n3 ->
      assert_no_duplicates "2" n3 >>= fun () ->

      with_node repo (fun g -> Graph.update g n0 ["b"] (normal kv1)) >>= fun n1 ->
      with_node repo (fun g -> Graph.update g n1 ["a"] (normal kv1)) >>= fun n2 ->
      with_node repo (fun g -> Graph.update g n2 ["a"] (normal kv1)) >>= fun n3 ->
      assert_no_duplicates "3" n3 >>= fun () ->

      with_node repo (fun g -> Graph.update g n0 ["a"] (normal kv1)) >>= fun n1 ->
      with_node repo (fun g -> Graph.update g n1 ["b"] (normal kv1)) >>= fun n2 ->
      with_node repo (fun g -> Graph.update g n2 ["b"] (normal kv1)) >>= fun n3 ->
      assert_no_duplicates "4" n3 >>= fun () ->

      Lwt.return_unit
    in
    run x test

  let test_commits x () =
    let test repo =

      let info date =
        let msg = Fmt.strf "Test commit: %d" date in
        Irmin.Info.v ~date:(Int64.of_int date) ~author:"test" msg
      in

      kv1 ~repo >>= fun kv1 ->
      let h = h repo and c = P.Repo.commit_t repo in

      let check_val = check (T.option P.Commit.Val.t) in
      let check_key = check P.Commit.Key.t in
      let check_keys = checks P.Commit.Key.t in

      (* t3 -a-> t2 -b-> t1 -x-> (v1) *)
      with_node repo (fun g -> Graph.v g ["x", normal kv1]) >>= fun kt1 ->
      with_node repo (fun g -> Graph.v g ["a", `Node kt1]) >>= fun kt2 ->
      with_node repo (fun g -> Graph.v g ["b", `Node kt2]) >>= fun kt3 ->

      (* r1 : t2 *)
      let with_info n fn = with_commit repo (fun h -> fn h ~info:(info n)) in
      with_info 3 (History.v ~node:kt2 ~parents:[]) >>= fun (kr1, _) ->
      with_info 3 (History.v ~node:kt2 ~parents:[]) >>= fun (kr1',_) ->
      P.Commit.find c kr1  >>= fun t1 ->
      P.Commit.find c kr1' >>= fun t1' ->
      check_val "t1" t1 t1';
      check_key "kr1" kr1 kr1';

      (* r1 -> r2 : t3 *)
      with_info 4 (History.v ~node:kt3 ~parents:[kr1]) >>= fun (kr2, _) ->
      with_info 4 (History.v ~node:kt3 ~parents:[kr1]) >>= fun (kr2',_) ->
      check_key "kr2" kr2 kr2';

      History.closure h ~min:[] ~max:[kr1] >>= fun kr1s ->
      check_keys "g1" [kr1] kr1s;

      History.closure h ~min:[] ~max:[kr2] >>= fun kr2s ->
      check_keys "g2" [kr1; kr2] kr2s;
      S.Commit.of_hash repo kr1 >|= function
      | None   -> Alcotest.fail "Cannot read commit hash"
      | Some c ->
          Alcotest.(check string) "author" "test" (Irmin.Info.author (S.Commit.info c))
    in
    run x test

  let test_branches x () =
    let test repo =
      let check_keys = checks S.Branch.t in
      let check_val = check (T.option @@ S.commit_t repo) in

      r1 ~repo >>= fun kv1 ->
      r2 ~repo >>= fun kv2 ->

      line "pre-update";
      S.Branch.set repo b1 kv1 >>= fun () ->
      line "post-update";
      S.Branch.find repo b1 >>= fun k1' ->
      check_val "r1" (Some kv1) k1';
      S.Branch.set repo b2 kv2 >>= fun () ->
      S.Branch.find repo b2 >>= fun k2' ->
      check_val "r2" (Some kv2) k2';
      S.Branch.set repo b1 kv2 >>= fun () ->
      S.Branch.find repo b1 >>= fun k2'' ->
      check_val "r1-after-update" (Some kv2) k2'';

      S.Branch.list repo >>= fun bs ->
      check_keys "list" [b1; b2] bs;
      S.Branch.remove repo b1 >>= fun () ->
      S.Branch.find repo b1 >>= fun empty ->
      check_val "empty" None empty;
      S.Branch.list repo >>= fun b2' ->
      check_keys "all-after-remove" [b2] b2';
      Lwt.return_unit
    in
    run x test

  let test_watch_exn x () =
    let test repo =
      S.master repo >>= fun t ->
      S.Head.find t >>= fun h ->
      let key = ["a"] in
      let v1  = "bar" in
      let v2  = "foo" in
      let r = ref 0 in
      let eq = Irmin.Type.(equal (Irmin.Diff.t @@ S.commit_t repo)) in
      let old_head = ref h in
      let check x =
        S.Head.get t >|= fun h2 ->
        match !old_head with
        | None   -> if eq (`Added h2) x then incr r
        | Some h -> if eq (`Updated (h, h2)) x then incr r
      in

      S.watch ?init:h t (fun v -> check v >|= fun () -> failwith "test")
      >>= fun u ->
      S.watch ?init:h t (fun v -> check v >>= fun () ->  Lwt.fail_with "test")
      >>= fun v ->
      S.watch ?init:h t (fun v -> check v)
      >>= fun w ->

      S.set_exn t ~info:(infof "update") key v1 >>= fun () ->
      retry (fun n -> Alcotest.(check int) ("watch 1 " ^ n) 3 !r) >>= fun () ->

      S.Head.find t >>= fun h ->
      old_head := h;

      S.set_exn t ~info:(infof "update") key v2 >>= fun () ->
      retry (fun n -> Alcotest.(check int) ("watch 2 " ^ n) 6 !r) >>= fun () ->

      S.unwatch u >>= fun () ->
      S.unwatch v >>= fun () ->
      S.unwatch w >>= fun () ->

      S.Head.get t >>= fun h ->
      old_head := Some h;

      S.watch_key ~init:h t key (fun _ -> incr r; failwith "test")
      >>= fun u ->
      S.watch_key ~init:h t key (fun _ -> incr r; Lwt.fail_with "test")
      >>= fun v ->
      S.watch_key ~init:h t key (fun _ -> incr r; Lwt.return_unit)
      >>= fun w ->
      S.set_exn t ~info:(infof "update") key v1 >>= fun () ->
      retry (fun n -> Alcotest.(check int) ("watch 3 " ^ n) 9 !r) >>= fun () ->
      S.set_exn t ~info:(infof "update") key v2 >>= fun () ->
      retry (fun n -> Alcotest.(check int) ("watch 4 " ^ n) 12 !r) >>= fun () ->
      S.unwatch u >>= fun () ->
      S.unwatch v >>= fun () ->
      S.unwatch w >>= fun () ->

      Alcotest.(check unit) "ok!" () ();
      Lwt.return_unit
    in
    run x test

  let test_watches x () =

    let pp_w ppf (p, w) = Fmt.pf ppf "%d/%d" p w in
    let pp_s ppf = function
      | None   -> Fmt.string ppf "*"
      | Some w -> pp_w ppf (w ())
    in

    let check_workers msg p w =
      match x.stats with
      | None       -> Lwt.return_unit
      | Some stats ->
        retry (fun s ->
            let got = stats () in
            let exp = p, w in
            let msg = Fmt.strf "workers: %s %a (%s)" msg pp_w got s in
            if got = exp then line msg
            else (
              Log.debug (fun f ->
                  f "check-worker: expected %a, got %a" pp_w exp pp_w got);
              Alcotest.failf "%s: %a / %a" msg pp_w got pp_w exp
            ))
    in

    let module State = struct
      type t = {
        mutable adds: int;
        mutable updates: int;
        mutable removes: int;
      }
      let empty () = { adds=0; updates=0; removes=0; }
      let add t = t.adds <- t.adds + 1
      let update t = t.updates <- t.updates + 1
      let remove t = t.removes <- t.removes + 1
      let pretty ppf t = Fmt.pf ppf "%d/%d/%d" t.adds t.updates t.removes
      let xpp ppf (a, u, r) = Fmt.pf ppf "%d/%d/%d" a u r
      let xadd (a, u, r) = (a+1, u, r)
      let xupdate (a, u, r) = (a, u+1, r)
      let xremove (a, u, r) = (a, u, r+1)

      let check ?sleep_t msg (p, w) a b =
        let pp ppf (a, u, r) =
          Fmt.pf ppf "{ adds=%d; updates=%d; removes=%d }" a u r
        in
        check_workers msg p w >>= fun () ->
        retry ?sleep_t (fun s ->
            let b = b.adds, b.updates, b.removes in
            let msg = Fmt.strf "state: %s (%s)" msg s in
            if a = b then line msg
            else Alcotest.failf "%s: %a / %a" msg pp a pp b
          )

      let process ?sleep_t t =
        function head ->
          begin match sleep_t with
            | None   -> Lwt.return_unit
            | Some s -> Lwt_unix.sleep s
          end >>= fun () ->
          let () = match head with
            | `Added _   -> add t
            | `Updated _ -> update t
            | `Removed _ -> remove t
          in
          Lwt.return_unit

      let apply msg state kind fn ?(first=false) on s n =
        let msg mode n w s =
          let kind = match kind with
            | `Add    -> "add"
            | `Update -> "update"
            | `Remove -> "remove"
          in
          let mode = match mode with `Pre -> "[pre-condition]" | `Post -> "" in
          Fmt.strf "%s %s %s %d on=%b expected=%a:%a current=%a:%a"
            mode msg kind n on xpp s pp_w w pretty state pp_s x.stats
        in
        let check mode n w s = check (msg mode n w s) w s state in
        let incr = match kind with
          | `Add -> xadd
          | `Update -> xupdate
          | `Remove -> xremove
        in
        let rec aux pre = function
          | 0 -> Lwt.return_unit
          | i ->
            let pre_w =
              if on then 1, (if i = n && first then 0 else 1) else 0, 0
            in
            let post_w = if on then 1, 1 else 0, 0 in
            let post = if on then incr pre else pre in
            check `Pre (n-i) pre_w pre >>= fun () -> (* check pre-condition *)
            Log.debug (fun f -> f "[waiting for] %s" (msg `Post (n-i) post_w post));
            fn (n-i) >>= fun () ->
            check `Post (n-i) post_w post >>= fun () -> (* check post-condition *)
            aux post (i-1)
        in
        aux s n

    end in

    let test repo =
      S.master repo >>= fun t1 ->
      S.Repo.v x.config >>= fun repo -> S.master repo >>= fun t2 ->

      Log.debug (fun f -> f "WATCH");

      let state = State.empty () in
      let sleep_t = 0.02 in
      let process = State.process ~sleep_t state in
      let stops_0 = ref [] in
      let stops_1 = ref [] in
      let rec watch = function
        | 0 -> Lwt.return_unit
        | n ->
          let t = if n mod 2 = 0 then t1 else t2 in
          S.watch t process >>= fun s ->
          if n mod 2 = 0 then stops_0 := s :: !stops_0
          else stops_1 := s :: !stops_1;
          watch (n-1)
      in
      let v1 = "X1" in
      let v2 = "X2" in

      S.set_exn t1 ~info:(infof "update") ["a";"b"] v1 >>= fun () ->
      S.Branch.remove repo S.Branch.master >>= fun () ->
      State.check "init" (0, 0) (0, 0, 0) state >>= fun () ->

      watch 100 >>= fun () ->

      State.check "watches on" (1, 0) (0, 0, 0) state >>= fun () ->

      S.set_exn t1 ~info:(infof "update") ["a";"b"] v1 >>= fun () ->
      State.check "watches adds" (1, 1) (100, 0, 0) state >>= fun () ->

      S.set_exn t2 ~info:(infof "update") ["a";"c"] v1 >>= fun () ->
      State.check "watches updates" (1, 1) (100, 100, 0) state >>= fun () ->

      S.Branch.remove repo S.Branch.master >>= fun () ->
      State.check "watches removes" (1, 1) (100, 100, 100) state >>= fun () ->

      Lwt_list.iter_s (fun f -> S.unwatch f) !stops_0 >>= fun () ->
      S.set_exn t2 ~info:(infof "update") ["a"] v1 >>= fun () ->
      State.check "watches half off" (1, 1) (150, 100, 100) state  >>= fun () ->

      Lwt_list.iter_s (fun f -> S.unwatch f) !stops_1 >>= fun () ->
      S.set_exn t1 ~info:(infof "update") ["a"] v2 >>= fun () ->
      State.check "watches off" (0, 0) (150, 100, 100) state >>= fun () ->

      Log.debug (fun f -> f "WATCH-ALL");
      let state = State.empty () in

      r1 ~repo >>= fun head ->
      let add = State.apply "branch-watch-all" state `Add (fun n ->
          let tag = Fmt.strf "t%d" n in
          S.Branch.set repo tag head
        ) in
      let remove = State.apply "branch-watch-all" state `Remove (fun n ->
          let tag = Fmt.strf "t%d" n in
          S.Branch.remove repo tag
        ) in

      S.Branch.watch_all repo (fun _ -> State.process state) >>= fun u ->

      add    true (0,  0, 0) 10 ~first:true >>= fun () ->
      remove true (10, 0, 0) 5 >>= fun () ->

      S.unwatch u  >>= fun () ->

      add    false (10, 0, 5) 4 >>= fun () ->
      remove false (10, 0, 5) 4 >>= fun () ->

      Log.debug (fun f -> f "WATCH-KEY");

      let state = State.empty () in
      let path1 = ["a"; "b"; "c"] in
      let path2 = ["a"; "d"] in
      let path3 = ["a"; "b"; "d"] in
      let add = State.apply "branch-key" state `Add (fun _ ->
          let v = "" in
          S.set_exn t1 ~info:(infof "set1") path1 v >>= fun () ->
          S.set_exn t1 ~info:(infof "set2") path2 v >>= fun () ->
          S.set_exn t1 ~info:(infof "set3") path3 v >>= fun () ->
          Lwt.return_unit
        ) in
      let update = State.apply "branch-key" state `Update (fun n ->
          let v = string_of_int n in
          S.set_exn t2 ~info:(infof "update1") path1 v >>= fun () ->
          S.set_exn t2 ~info:(infof "update2") path2 v >>= fun () ->
          S.set_exn t2 ~info:(infof "update3") path3 v >>= fun () ->
          Lwt.return_unit
        ) in
      let remove = State.apply "branch-key" state `Remove (fun _ ->
          S.remove_exn t1 ~info:(infof "remove1") path1 >>= fun () ->
          S.remove_exn t1 ~info:(infof "remove2") path2 >>= fun () ->
          S.remove_exn t1 ~info:(infof "remove3") path3 >>= fun () ->
          Lwt.return_unit
        ) in

      S.remove_exn t1 ~info:(infof "clean") [] >>= fun () ->
      S.Head.get t1 >>= fun init ->

      S.watch_key t1 ~init path1 (State.process state) >>= fun u ->

      add    true (0, 0 , 0) 1  ~first:true >>= fun () ->
      update true (1, 0 , 0) 10 >>= fun () ->
      remove true (1, 10, 0) 1  >>= fun () ->

      S.unwatch u >>= fun () ->

      add    false (1, 10, 1) 3 >>= fun () ->
      update false (1, 10, 1) 5 >>= fun () ->
      remove false (1, 10, 1) 4 >>= fun () ->

      Log.debug (fun f -> f "WATCH-MORE");

      let state = State.empty () in

      let update = State.apply "watch-more" state `Update (fun n ->
          let v = string_of_int n in
          let path1 = ["a"; "b"; "c"; string_of_int n; "1"] in
          let path2 = ["a"; "x"; "c"; string_of_int n; "1"] in
          let path3 = ["a"; "y"; "c"; string_of_int n; "1"] in
          S.set_exn t2 ~info:(infof "update1") path1 v >>= fun () ->
          S.set_exn t2 ~info:(infof "update2") path2 v >>= fun () ->
          S.set_exn t2 ~info:(infof "update3") path3 v >>= fun () ->
          Lwt.return_unit
        ) in

      S.remove_exn t1 ~info:(infof "remove") ["a"] >>= fun () ->
      S.set_exn t1 ~info:(infof "prepare") ["a";"b";"c"] "" >>= fun () ->

      S.Head.get t1 >>= fun h ->
      S.watch_key t2 ~init:h ["a";"b"] (State.process state) >>= fun u ->

      update true (0, 0, 0) 10 ~first:true >>= fun () ->
      S.unwatch u >>= fun () ->
      update false (0, 10, 0) 10 >>= fun () ->

      Lwt.return_unit
    in
    run x test

  let test_simple_merges x () =

    (* simple merges *)
    let check_merge () =
      let ok = Irmin.Merge.ok in
      let dt = T.(option int64) in
      let dx = T.(list (pair string int64)) in
      let merge_skip ~old:_ _ _ = ok None in
      let merge_left ~old:_ x _ = ok x in
      let merge_right ~old:_ _ y = ok y in
      let merge_default = Irmin.Merge.default dt in
      let merge = function
        | "left"  -> Irmin.Merge.v dt merge_left
        | "right" -> Irmin.Merge.v dt merge_right
        | "skip"  -> Irmin.Merge.v dt merge_skip
        | _ -> merge_default
      in
      let merge_x = Irmin.Merge.alist T.string T.int64 merge in
      let old () = ok (Some [ "left", 1L; "foo", 2L; ]) in
      let x =   [ "left", 2L; "right", 0L] in
      let y =   [ "left", 1L; "bar"  , 3L; "skip", 0L ] in
      let m =   [ "left", 2L; "bar"  , 3L] in
      Irmin.Merge.(f merge_x) ~old x y >>= function
      | Error (`Conflict c) -> Alcotest.failf "conflict %s" c
      | Ok m'               ->
        check dx "compound merge" m m';
        Lwt.return_unit
    in

    let test repo =
      check_merge () >>= fun () ->
      kv1 ~repo >>= fun kv1 ->
      kv2 ~repo >>= fun kv2 ->
      let check_result =
        check (Irmin.Merge.result_t T.(option P.Contents.Key.t))
      in

      (* merge contents *)

      with_contents repo (fun v ->
          Irmin.Merge.f (P.Contents.merge v)
            ~old:(old (Some kv1)) (Some kv1) (Some kv1))
      >>= fun kv1' ->
      check_result "merge kv1" (Ok (Some kv1)) kv1';

      with_contents repo (fun v ->
          Irmin.Merge.f (P.Contents.merge v)
            ~old:(old (Some kv1)) (Some kv1) (Some kv2))
      >>= fun kv2' ->
      check_result "merge kv2" (Ok (Some kv2)) kv2';

      (* merge nodes *)

      let g = g repo in

      (* The empty node *)
      with_node repo (fun g -> Graph.v g []) >>= fun k0 ->

      (* Create the node t1 -x-> (v1) *)
      with_node repo (fun g -> Graph.v g ["x", normal kv1]) >>= fun k1 ->

      (* Create the node t2 -b-> t1 -x-> (v1) *)
      with_node repo (fun g -> Graph.v g ["b", `Node k1]) >>= fun k2 ->

      (* Create the node t3 -c-> t1 -x-> (v1) *)
      with_node repo (fun g -> Graph.v g ["c", `Node k1]) >>= fun k3 ->

      (* Should create the node:
                          t4 -b-> t1 -x-> (v1)
                             \c/ *)
      with_node repo (fun g ->
          Irmin.Merge.(f @@ P.Node.merge g)
            ~old:(old (Some k0)) (Some k2) (Some k3))
      >>= fun k4 ->
      merge_exn "k4" k4 >>= fun k4 ->
      let k4 = match k4 with Some k -> k | None -> failwith "k4" in

      let _ = k4 in

      let succ_t = T.(pair string Graph.value_t) in

      Graph.list g k4 >>= fun succ ->
      checks succ_t "k4"[ ("b", `Node k1); ("c", `Node k1) ] succ;

      let info date =
        let i = Int64.of_int date in
        Irmin.Info.v ~date:i ~author:"test" "Test commit"
      in

      let c = P.Repo.commit_t repo in
      let with_info n fn = with_commit repo (fun h -> fn h ~info:(info n)) in

      with_info 0 (History.v ~node:k0 ~parents:[]) >>= fun (kr0, _) ->
      with_info 1 (History.v ~node:k2 ~parents:[kr0]) >>= fun (kr1, _) ->
      with_info 2 (History.v ~node:k3 ~parents:[kr0]) >>= fun (kr2, _) ->
      with_info 3 (fun h ~info ->
          Irmin.Merge.f (History.merge h ~info:(fun () -> info))
            ~old:(old kr0) kr1 kr2)
      >>= fun kr3 ->
      merge_exn "kr3" kr3 >>= fun kr3 ->

      with_info 4 (fun h ~info ->
          Irmin.Merge.f (History.merge h ~info:(fun () -> info))
            ~old:(old kr2) kr2 kr3)
      >>= fun kr3_id' ->
      merge_exn "kr3_id'" kr3_id' >>= fun kr3_id' ->
      check S.Hash.t "kr3 id with immediate parent'" kr3 kr3_id';

      with_info 5 (fun h ~info ->
          Irmin.Merge.f (History.merge h ~info:(fun () -> info))
            ~old:(old kr0) kr0 kr3)
      >>= fun kr3_id ->
      merge_exn "kr3_id" kr3_id >>= fun kr3_id ->
      check S.Hash.t "kr3 id with old parent" kr3 kr3_id;

      with_info 3 @@ History.v ~node:k4 ~parents:[kr1; kr2] >>= fun (kr3', _) ->

      P.Commit.find c kr3 >>= fun r3 ->
      P.Commit.find c kr3' >>= fun r3' ->
      check T.(option P.Commit.Val.t) "r3" r3 r3';
      check S.Hash.t "kr3" kr3 kr3';
      Lwt.return_unit
    in
    run x test

  let test_history x () =
    let test repo =
      let info date =
        let i = Int64.of_int date in
        Irmin.Info.v ~date:i ~author:"test" "Test commit"
      in
      let tree = S.Tree.empty in
      let assert_lcas_err msg err l2 =
        let err_str = function
          | `Too_many_lcas    -> "Too_many_lcas"
          | `Max_depth_reached -> "Max_depth_reached"
        in
        let pp_commits = Fmt.Dump.(list S.Commit.pp_hash) in
        let l2 = match l2 with
          | Ok x    -> Alcotest.failf "%s: %a" msg pp_commits x
          | Error e -> err_str e
        in
        Alcotest.(check string) msg (err_str err) l2
      in
      let assert_lcas msg l1 l2 =
        let l2 = match l2 with
          | Ok x -> x
          | Error `Too_many_lcas     -> Alcotest.failf "%s: Too many LCAs" msg
          | Error `Max_depth_reached -> Alcotest.failf "%s: max depth reached" msg
        in
        checks (S.commit_t repo) msg l1 l2
      in
      let assert_lcas msg ~max_depth n a b expected =
        S.of_commit a >>= fun a ->
        S.of_commit b >>= fun b ->
        S.lcas ~max_depth ~n a b >>= fun lcas ->
        assert_lcas msg expected lcas;
        S.lcas ~max_depth:(max_depth - 1) ~n a b >>= fun lcas ->
        let msg = Printf.sprintf "%s [max-depth=%d]" msg (max_depth - 1) in
        assert_lcas_err msg `Max_depth_reached lcas;
        Lwt.return_unit
      in

      (* test that we don't compute too many lcas

         0->1->2->3->4

      *)
      S.Commit.v repo ~info:(info 0) ~parents:[]   tree >>= fun k0 ->
      S.Commit.v repo ~info:(info 1) ~parents:[k0] tree >>= fun k1 ->
      S.Commit.v repo ~info:(info 2) ~parents:[k1] tree >>= fun k2 ->
      S.Commit.v repo ~info:(info 3) ~parents:[k2] tree >>= fun k3 ->
      S.Commit.v repo ~info:(info 4) ~parents:[k3] tree >>= fun k4 ->

      assert_lcas "line lcas 1" ~max_depth:0 3 k3 k4 [k3] >>= fun () ->
      assert_lcas "line lcas 2" ~max_depth:1 3 k2 k4 [k2] >>= fun () ->
      assert_lcas "line lcas 3" ~max_depth:2 3 k1 k4 [k1] >>= fun () ->

      (* test for multiple lca

         4->10--->11-->13-->15
             |      \______/___
             |       ____/     \
             |      /           \
             \--->12-->14-->16-->17

      *)
      S.Commit.v repo ~info:(info 10) ~parents:[k4]       tree >>= fun k10 ->
      S.Commit.v repo ~info:(info 11) ~parents:[k10]      tree >>= fun k11 ->
      S.Commit.v repo ~info:(info 12) ~parents:[k10]      tree >>= fun k12 ->
      S.Commit.v repo ~info:(info 13) ~parents:[k11]      tree >>= fun k13 ->
      S.Commit.v repo ~info:(info 14) ~parents:[k12]      tree >>= fun k14 ->
      S.Commit.v repo ~info:(info 15) ~parents:[k12; k13] tree >>= fun k15 ->
      S.Commit.v repo ~info:(info 16) ~parents:[k14]      tree >>= fun k16 ->
      S.Commit.v repo ~info:(info 17) ~parents:[k11; k16] tree >>= fun k17 ->

      assert_lcas "x lcas 0" ~max_depth:0 5 k10 k10 [k10]      >>= fun () ->
      assert_lcas "x lcas 1" ~max_depth:0 5 k14 k14 [k14]      >>= fun () ->
      assert_lcas "x lcas 2" ~max_depth:0 5 k10 k11 [k10]      >>= fun () ->
      assert_lcas "x lcas 3" ~max_depth:1 5 k12 k16 [k12]      >>= fun () ->
      assert_lcas "x lcas 4" ~max_depth:1 5 k10 k13 [k10]      >>= fun () ->
      assert_lcas "x lcas 5" ~max_depth:2 5 k13 k14 [k10]      >>= fun () ->
      assert_lcas "x lcas 6" ~max_depth:3 5 k15 k16 [k12]      >>= fun () ->
      assert_lcas "x lcas 7" ~max_depth:3 5 k15 k17 [k11; k12] >>= fun () ->

      (* lcas on non transitive reduced graphs

                  /->16
                 |
         4->10->11->12->13->14->15
                 |        \--|--/
                 \-----------/
      *)
      S.Commit.v repo ~info:(info 10) ~parents:[k4]      tree >>= fun k10 ->
      S.Commit.v repo ~info:(info 11) ~parents:[k10]     tree >>= fun k11 ->
      S.Commit.v repo ~info:(info 12) ~parents:[k11]     tree >>= fun k12 ->
      S.Commit.v repo ~info:(info 13) ~parents:[k12]     tree >>= fun k13 ->
      S.Commit.v repo ~info:(info 14) ~parents:[k11;k13] tree >>= fun k14 ->
      S.Commit.v repo ~info:(info 15) ~parents:[k13;k14] tree >>= fun k15 ->
      S.Commit.v repo ~info:(info 16) ~parents:[k11]     tree >>= fun k16 ->

      assert_lcas "weird lcas 1" ~max_depth:0 3 k14 k15 [k14] >>= fun () ->
      assert_lcas "weird lcas 2" ~max_depth:0 3 k13 k15 [k13] >>= fun () ->
      assert_lcas "weird lcas 3" ~max_depth:1 3 k12 k15 [k12] >>= fun () ->
      assert_lcas "weird lcas 4" ~max_depth:1 3 k11 k15 [k11] >>= fun () ->
      assert_lcas "weird lcas 4" ~max_depth:3 3 k15 k16 [k11] >>= fun () ->

      (* fast-forward *)
      let ff = testable (Irmin.Type.(result unit S.ff_error_t)) in

      S.of_commit k12 >>= fun t12  ->
      S.Head.fast_forward t12 k16 >>= fun b1 ->
      Alcotest.(check ff) "ff 1.1" (Error `Rejected) b1;
      S.Head.get t12 >>= fun k12' ->
      check (S.commit_t repo) "ff 1.2" k12 k12';

      S.Head.fast_forward t12 ~n:1 k14 >>= fun b2 ->
      Alcotest.(check ff) "ff 2.1" (Error `Rejected) b2;
      S.Head.get t12 >>= fun k12'' ->
      check (S.commit_t repo) "ff 2.2" k12 k12'';

      S.Head.fast_forward t12 k14 >>= fun b3 ->
      Alcotest.(check ff) "ff 2.2" (Ok ()) b3;
      S.Head.get t12 >>= fun k14' ->
      check (S.commit_t repo) "ff 2.3" k14 k14';

      Lwt.return_unit
    in
    run x test

  let test_empty x () =
    let test repo =
      S.empty repo >>= fun t ->
      S.Head.find t >>= fun h ->
      check T.(option @@ S.commit_t repo) "empty" None h;
      r1 ~repo >>= fun r1 ->
      S.set_exn t ~info:Irmin.Info.none ["b"; "x"] v1 >>= fun () ->
      S.Head.find t >>= fun h ->
      check T.(option @@ S.commit_t repo) "not empty" (Some r1) h;
      Lwt.return_unit
    in
    run x test

  let test_slice x () =
    let test repo =
      S.master repo >>= fun t ->
      let a = "" in
      let b = "haha" in
      S.set_exn t ~info:(infof "slice") ["x";"a"] a >>= fun () ->
      S.set_exn t ~info:(infof "slice") ["x";"b"] b >>= fun () ->
      S.Repo.export repo >>= fun slice ->
      let str = T.to_json_string P.Slice.t slice in
      let slice' =
        match T.decode_json P.Slice.t (Jsonm.decoder (`String str)) with
        | Ok t          -> t
        | Error (`Msg e) -> Alcotest.failf "decoding error: %s" e
      in
      check P.Slice.t "slices" slice slice';

      Lwt.return_unit
    in
    run x test

  let test_private_nodes x () =
    let test repo =
      let check_val = check T.(option S.contents_t) in
      let vx = "VX" in
      let vy = "VY" in
      S.master repo >>= fun t ->
      S.set_exn t ~info:(infof "add x/y/z") ["x";"y";"z"] vx >>= fun () ->
      S.get_tree t ["x"] >>= fun tree ->
      S.set_tree_exn t ~info:(infof "update") ["u"] tree >>= fun () ->
      S.find t ["u";"y";"z"] >>= fun vx' ->
      check_val "vx" (Some vx) vx';

      S.get_tree t ["u"] >>= fun tree1 ->
      S.set_exn t ~info:(infof "add u/x/y") ["u";"x";"y"] vy >>= fun () ->
      S.get_tree t ["u"] >>= fun tree2 ->
      S.Tree.add tree ["x";"z"] vx >>= fun tree3 ->
      Irmin.Merge.f S.Tree.merge ~old:(Irmin.Merge.promise tree1) tree2 tree3
      >>= merge_exn "tree" >>= fun v' ->
      S.set_tree_exn t ~info:(infof "merge") ["u"] v' >>= fun () ->
      S.find t ["u";"x";"y"] >>= fun vy' ->
      check_val "vy after merge" (Some vy) vy';

      S.find t ["u";"x";"z"] >>= fun vx' ->
      check_val "vx after merge" (Some vx) vx';
      Lwt.return_unit
    in
    run x test

  let test_stores x () =
    let test repo =
      let check_val = check T.(option S.contents_t) in
      let check_list = checks T.(pair S.Key.step_t S.kind_t) in
      S.master repo >>= fun t ->
      S.set_exn t ~info:(infof "init") ["a";"b"] v1 >>= fun () ->
      S.mem t ["a";"b"] >>= fun b0 ->
      Alcotest.(check bool) "mem0" true b0;
      S.clone ~src:t ~dst:"test" >>= fun t ->
      S.mem t ["a";"b"] >>= fun b1 ->
      Alcotest.(check bool) "mem1" true b1;
      S.mem t ["a"] >>= fun b2 ->
      Alcotest.(check bool) "mem2" false b2;
      S.find t ["a";"b"] >>= fun v1' ->
      check_val "v1.1" (Some v1) v1';

      S.Head.get t >>= fun r1 ->
      S.clone ~src:t ~dst:"test" >>= fun t ->

      S.set_exn t ~info:(infof "update") ["a";"c"] v2 >>= fun () ->
      S.mem t ["a";"b"] >>= fun b1 ->
      Alcotest.(check bool) "mem3" true b1;
      S.mem t ["a"] >>= fun b2 ->
      Alcotest.(check bool) "mem4" false b2;
      S.find t ["a";"b"] >>= fun v1' ->
      check_val "v1.1" (Some v1) v1';
      S.mem t ["a";"c"] >>= fun b1 ->
      Alcotest.(check bool) "mem5" true b1;
      S.find t ["a";"c"] >>= fun v2' ->
      check_val "v1.1" (Some v2) v2';

      S.remove_exn t ~info:(infof "remove") ["a";"b"] >>= fun () ->
      S.find t ["a";"b"] >>= fun v1''->
      check_val "v1.2" None v1'';
      S.Head.set t r1 >>= fun () ->
      S.find t ["a";"b"] >>= fun v1''->
      check_val "v1.3" (Some v1) v1'';
      S.list t ["a"] >>= fun ks ->
      check_list "path" ["b", `Contents] ks;

      S.set_exn t ~info:(infof "update2") ["a"; long_random_ascii_string] v1
      >>= fun () ->

      S.remove_exn t ~info:(infof "remove rec") ["a"] >>= fun () ->
      S.list t [] >>= fun dirs ->
      check_list "remove rec" [] dirs;

      Lwt.catch
        (fun () ->
           S.set_exn t ~info:(infof "update root") [] v1 >>= fun () ->
           Alcotest.fail "update root")
        (function
          | Invalid_argument _ -> Lwt.return_unit
          | e -> Alcotest.fail ("update root: " ^ Printexc.to_string e))
      >>= fun () ->
      S.find t [] >>= fun none ->
      check_val "read root" none None;

      S.set_exn t ~info:(infof "update") ["a"] v1 >>= fun () ->
      S.remove_exn t ~info:(infof "remove rec --all") [] >>= fun () ->
      S.list t [] >>= fun dirs ->
      check_list "remove rec root" [] dirs;

      let a = "ok" in
      let b = "maybe?" in

      S.set_exn t ~info:(infof "fst one") ["fst"] a        >>= fun () ->
      S.set_exn t ~info:(infof "snd one") ["fst"; "snd"] b >>= fun () ->

      S.find t ["fst"] >>= fun fst ->
      check_val "data model 1" None fst;
      S.find t ["fst"; "snd"] >>= fun snd ->
      check_val "data model 2" (Some b) snd;

      S.set_exn t ~info:(infof "fst one") ["fst"] a >>= fun () ->

      S.find t ["fst"] >>= fun fst ->
      check_val "data model 3" (Some a) fst;
      S.find t ["fst"; "snd"] >>= fun snd ->
      check_val "data model 4" None snd;

      let tagx = "x" in
      let tagy = "y" in
      let xy = ["x";"y"] in
      let vx = "VX" in
      S.of_branch repo tagx >>= fun tx ->
      S.Branch.remove repo tagx >>= fun () ->
      S.Branch.remove repo tagy >>= fun () ->

      S.set_exn tx ~info:(infof "update") xy vx >>= fun () ->
      S.clone ~src:tx ~dst:tagy >>= fun ty ->
      S.find ty xy >>= fun vx' ->
      check_val "update tag" (Some vx) vx';

      S.status tx |> fun tagx' ->
      S.status ty |> fun tagy' ->
      check (S.Status.t repo) "tagx" (`Branch tagx) tagx';
      check (S.Status.t repo) "tagy" (`Branch tagy) tagy';

      Lwt.return_unit
    in
    run x test

  let stats_t = Alcotest.testable S.Tree.pp_stats (=)
  let empty_stats = { S.Tree.nodes=0; leafs=0; skips=0; depth=0; width=0 }

  let test_trees x () =
    let test repo =
      S.master repo >>= fun t ->
      let nodes = random_nodes 100 in
      let foo1 = random_value 10 in
      let foo2 = random_value 10 in


      (* Testing [Tree.remove] *)

      S.Tree.empty |> fun v1 ->
      S.Tree.stats v1 >>= fun s ->
      Alcotest.(check stats_t) "empty stats" empty_stats s;

      S.Tree.add v1 ["foo";"1"] foo1 >>= fun v1 ->
      S.Tree.add v1 ["foo";"2"] foo2 >>= fun v1 ->

      S.Tree.stats v1 >>= fun s ->
      Alcotest.(check stats_t) "stats 1"
        { S.Tree.nodes=2; leafs=2; skips=0; depth=2; width=2 } s;

      S.Tree.remove v1 ["foo";"1"] >>= fun v1 ->
      S.Tree.remove v1 ["foo";"2"] >>= fun v1 ->

      S.Tree.stats v1 >>= fun s ->
      Alcotest.(check stats_t) "empty stats" empty_stats s;

      S.set_tree_exn t ~info:(infof "empty tree") [] v1 >>= fun () ->
      S.Head.get t >>= fun head ->
      S.Commit.hash head |> fun head ->
      P.Commit.find (ct repo) head >>= fun commit ->
      let node = P.Commit.Val.node (get commit) in
      P.Node.find (n repo) node >>= fun node ->
      check T.(option P.Node.Val.t) "empty tree" (Some P.Node.Val.empty) node;

      (* Testing [Tree.diff] *)

      let contents = T.pair S.contents_t S.metadata_t in
      let diff = T.(pair S.key_t (Irmin.Diff.t contents)) in
      let check_diffs = checks diff in
      let check_val = check T.(option contents) in
      let check_ls = check T.(list (pair S.step_t S.kind_t)) in
      let normal c = Some (c, S.Metadata.default) in
      let d0 = S.Metadata.default in

      S.Tree.empty |> fun v0 ->
      S.Tree.empty |> fun v1 ->
      S.Tree.empty |> fun v2 ->
      S.Tree.add v1 ["foo";"1"] foo1 >>= fun v1 ->
      S.Tree.find_all v1 ["foo"; "1"] >>= fun f ->
      check_val "tree update" (normal foo1) f;

      S.Tree.add v2 ["foo";"1"] foo2 >>= fun v2 ->
      S.Tree.add v2 ["foo";"2"] foo1 >>= fun v2 ->

      S.Tree.diff v0 v1 >>= fun d1 ->
      check_diffs "diff 1" [ ["foo";"1"], `Added (foo1, d0) ] d1;

      S.Tree.diff v1 v0 >>= fun d2 ->
      check_diffs "diff 2" [ ["foo";"1"], `Removed (foo1, d0) ] d2;

      S.Tree.diff v1 v2 >>= fun d3 ->
      check_diffs "diff 3" [ ["foo";"1"], `Updated ((foo1, d0), (foo2, d0));
                             ["foo";"2"], `Added (foo1, d0)] d3;

      S.Tree.add v2 ["foo"; "bar"; "1"] foo1 >>= fun v3 ->
      S.Tree.diff v2 v3 >>= fun d4 ->
      check_diffs "diff 4" [ ["foo"; "bar"; "1" ], `Added (foo1, d0) ] d4;
      S.Tree.diff v3 v2 >>= fun d5 ->
      check_diffs "diff 4" [ ["foo"; "bar"; "1" ], `Removed (foo1, d0) ] d5;

      (* Testing concrete representation *)

      let c0 = S.Tree.empty in
      S.Tree.add c0 ["foo"; "a"] "1" >>= fun c0 ->
      S.Tree.add c0 ["foo"; "b"; "c"] "2" >>= fun c0 ->
      S.Tree.add c0 ["bar"; "d"] "3" >>= fun c0 ->
      S.Tree.add c0 ["e"] "4" >>= fun c0 ->
      S.Tree.to_concrete c0 >>= fun t0 ->
      let t0 = S.Tree.of_concrete t0 in
      S.Tree.diff c0 t0 >>= fun d0 ->
      check_diffs "concrete roundtrip" [] d0;
      S.Tree.list c0 [] >>= fun c0' ->
      S.Tree.list t0 [] >>= fun t0' ->
      check_ls "concrete list /" c0' t0';
      S.Tree.list c0 ["foo"] >>= fun c0' ->
      S.Tree.list t0 ["foo"] >>= fun t0' ->
      check_ls "concrete tree list /foo" c0' t0';
      S.Tree.list c0 ["bar"; "d"] >>= fun c0' ->
      S.Tree.list t0 ["bar"; "d"] >>= fun t0' ->
      check_ls "concrete tree list /bar/d" c0' t0';

      (* Testing other tree operations. *)

      S.Tree.empty |> fun v0 ->
      S.Tree.to_concrete v0 >>= fun c ->
      (match c with
        | `Tree [] -> ()
        | _ -> Alcotest.fail "Excpected empty tree");
      S.Tree.add v0 [] foo1 >>= fun v0 ->
      S.Tree.find_all v0 [] >>= fun foo1' ->
      check_val "read /" (normal foo1) foo1';

      S.Tree.add v0 ["foo";"1"] foo1 >>= fun v0 ->
      S.Tree.find_all v0 ["foo";"1"] >>= fun foo1' ->
      check_val "read foo/1" (normal foo1) foo1';

      S.Tree.add v0 ["foo";"2"] foo2 >>= fun v0 ->
      S.Tree.find_all v0 ["foo";"2"] >>= fun foo2' ->
      check_val "read foo/2" (normal foo2) foo2';

      let check_tree v =
        S.Tree.list v ["foo"] >>= fun ls ->
        check_ls "path1" [ ("1", `Contents); ("2", `Contents) ] ls;
        S.Tree.find_all v ["foo";"1"] >>= fun foo1' ->
        check_val "foo1" (normal foo1) foo1';
        S.Tree.find_all v ["foo";"2"] >>= fun foo2' ->
        check_val "foo2" (normal foo2) foo2';
        Lwt.return_unit
      in

      Lwt_list.fold_left_s (fun v0 (k,v) ->
          S.Tree.add v0 k v
        ) v0 nodes >>= fun v0 ->
      check_tree v0 >>= fun () ->

      S.set_tree_exn t ~info:(infof "update_path b/") ["b"] v0 >>= fun () ->
      S.set_tree_exn t ~info:(infof "update_path a/") ["a"] v0 >>= fun () ->

      S.list t ["b";"foo"] >>= fun ls ->
      check_ls "path2" [ "1", `Contents; "2", `Contents] ls;
      S.find_all t ["b";"foo";"1"] >>= fun foo1' ->
      check_val "foo1" (normal foo1) foo1';
      S.find_all t ["a";"foo";"2"] >>= fun foo2' ->
      check_val "foo2" (normal foo2) foo2';

      S.get_tree t ["b"] >>= fun v0 ->
      check_tree v0 >>= fun () ->

      S.set_exn t ~info:(infof "update b/x") ["b";"x"] foo1 >>= fun () ->
      S.get_tree t ["b"] >>= fun v2 ->
      S.Tree.add v0 ["y"] foo2 >>= fun v1 ->

      Irmin.Merge.(f S.Tree.merge ~old:(promise v0) v1 v2) >>=
      merge_exn "merge trees" >>= fun v' ->

      S.set_tree_exn t ~info:(infof "merge_path") ["b"] v' >>= fun () ->
      S.find_all t ["b";"x"] >>= fun foo1' ->
      S.find_all t ["b";"y"] >>= fun foo2' ->
      check_val "merge: b/x" (normal foo1) foo1';
      check_val "merge: b/y" (normal foo2) foo2';

      Lwt_list.iteri_s (fun i (k, v) ->
          S.find_all t ("a" :: k) >>= fun v' ->
          check_val ("a"^string_of_int i) (normal v) v';
          S.find_all t ("b" ::  k) >>= fun v' ->
          check_val ("b"^string_of_int i) (normal v) v';
          Lwt.return_unit
        ) nodes >>= fun () ->

      S.get_tree t ["b"] >>= fun v2 ->
      S.Tree.find_all v2 ["foo"; "1"] >>= fun _ ->
      S.Tree.add v2 ["foo"; "1"] foo2 >>= fun v2 ->
      S.set_tree_exn t ~info:(infof"v2") ["b"] v2 >>= fun () ->
      S.find_all t ["b";"foo";"1"] >>= fun foo2' ->
      check_val "update tree" (normal foo2) foo2';

      S.get_tree t ["b"] >>= fun v3 ->
      S.Tree.find_all v3 ["foo"; "1"] >>= fun _ ->
      S.Tree.remove v3 ["foo"; "1"] >>= fun v3 ->
      S.set_tree_exn t ~info:(infof "v3") ["b"] v3 >>= fun () ->
      S.find_all t ["b";"foo";"1"] >>= fun foo2' ->
      check_val "remove tree" None foo2';

      r1 ~repo >>= fun r1 ->
      r2 ~repo >>= fun r2 ->
      let i0 = Irmin.Info.empty in
      S.Commit.v repo ~info:Irmin.Info.empty ~parents:[r1;r2] v3 >>= fun c ->
      S.Head.set t c >>= fun () ->
      S.Head.get t >>= fun h ->

      S.Commit.info h |> fun i ->
      check Irmin.Info.t "commit info" i0 i;

      S.of_commit h >>= fun tt ->
      S.history tt >>= fun g ->
      let pred = S.History.pred g h in
      checks (S.commit_t repo) "head" [r1;r2] pred;

      S.find_all tt ["b";"foo";"1"] >>= fun foo2'' ->
      check_val "remove tt" None foo2'';

      let vx = "VX" in
      let px = ["x";"y";"z"] in
      S.set_exn tt ~info:(infof "update") px vx >>= fun () ->
      S.get_tree tt [] >>= fun tree ->
      S.Tree.clear_caches tree;

      S.Tree.stats tree >>= fun s ->
      Alcotest.(check stats_t) "lazy stats"
        { S.Tree.nodes=0; leafs=0; skips=1; depth=0; width=0 } s;

      S.Tree.stats ~force:true tree >>= fun s ->
      Alcotest.(check stats_t) "forced stats"
        { S.Tree.nodes=404; leafs=103; skips=0; depth=5; width=103 } s;

      S.Tree.find_all tree px >>= fun vx' ->
      check_val "updates" (normal vx) vx';

      S.Tree.empty |> fun v ->
      S.Tree.add v [] vx >>= fun v ->
      S.set_tree_exn t ~info:(infof "update file as tree") ["a"] v >>= fun () ->
      S.find_all t ["a"] >>= fun vx' ->
      check_val "update file as tree" (normal vx) vx';
      Lwt.return_unit
    in
    run x test

  module Sync = Irmin.Sync(S)

  let test_sync x () =
    let test repo =
      S.master repo >>= fun t1 ->

      S.set_exn t1 ~info:(infof "update a/b") ["a";"b"] v1 >>= fun () ->
      S.Head.get t1 >>= fun h ->
      S.Head.get t1 >>= fun _r1 ->
      S.set_exn t1 ~info:(infof "update a/c") ["a";"c"] v2 >>= fun () ->
      S.Head.get t1 >>= fun r2 ->
      S.set_exn t1 ~info:(infof "update a/d") ["a";"d"] v1 >>= fun () ->
      S.Head.get t1 >>= fun _r3 ->

      S.history t1 ~min:[h] >>= fun h ->
      Alcotest.(check int) "history-v" 3 (S.History.nb_vertex h);
      Alcotest.(check int) "history-e" 2 (S.History.nb_edges h);

      let remote = Irmin.remote_store (module S) t1 in

      Sync.fetch_exn t1 ~depth:0 remote >>= fun partial ->
      Sync.fetch_exn t1 remote >>= fun full ->

      (* Restart a fresh store and import everything in there. *)
      let tag = "export" in
      S.of_branch repo tag >>= fun t2 ->
      S.Head.set t2 partial >>= fun () ->

      S.mem t2 ["a";"b"] >>= fun b1 ->
      Alcotest.(check bool) "mem-ab" true b1;

      S.mem t2 ["a";"c"] >>= fun b2 ->
      Alcotest.(check bool) "mem-ac" true b2;

      S.mem t2 ["a";"d"] >>= fun b3 ->
      Alcotest.(check bool) "mem-ad" true b3;
      S.get t2 ["a";"d"] >>= fun v1' ->
      check S.contents_t "v1" v1 v1';

      S.Head.set t2 r2 >>= fun () ->
      S.mem t2 ["a";"d"] >>= fun b4 ->
      Alcotest.(check bool) "mem-ab" false b4;

      S.Head.set t2 full >>= fun () ->
      S.Head.set t2 r2 >>= fun () ->
      S.mem t2 ["a";"d"] >>= fun b4 ->
      Alcotest.(check bool) "mem-ad" false b4;
      Lwt.return_unit
    in
    run x test

  module Dot = Irmin.Dot(S)

  let output_file t file =
    let buf = Buffer.create 1024 in
    let date d =
      let tm = Unix.localtime (Int64.to_float d) in
      Fmt.strf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in
    Dot.output_buffer t ~date buf >>= fun () ->
    let oc = open_out_bin (file ^ ".dot") in
    output_string oc (Buffer.contents buf);
    close_out oc;
    Lwt.return_unit

  let test_merge x () =
    let test repo =
      let v1 = "X1" in
      let v2 = "X2" in
      let v3 = "X3" in

      S.master repo >>= fun t1 ->

      S.set_exn t1 ~info:(infof "update a/b/a") ["a";"b";"a"] v1 >>= fun () ->
      S.set_exn t1 ~info:(infof "update a/b/b") ["a";"b";"b"] v2 >>= fun () ->
      S.set_exn t1 ~info:(infof "update a/b/c") ["a";"b";"c"] v3 >>= fun () ->

      let test = "test" in

      S.clone ~src:t1 ~dst:test >>= fun t2 ->

      S.set_exn t1 ~info:(infof "update master:a/b/b") ["a";"b";"b"] v1 >>= fun () ->
      S.set_exn t1 ~info:(infof "update master:a/b/b") ["a";"b";"b"] v3 >>= fun () ->
      S.set_exn t2 ~info:(infof "update test:a/b/c")   ["a";"b";"c"] v1 >>= fun () ->

      output_file t1 "before" >>= fun () ->
      S.merge_into ~info:(infof "merge test into master") t2 ~into:t1 >>= fun m ->
      merge_exn "m" m >>= fun () ->
      output_file t1 "after" >>= fun () ->

      S.get t1 ["a";"b";"c"] >>= fun v1' ->
      S.get t2 ["a";"b";"b"] >>= fun v2' ->
      S.get t1 ["a";"b";"b"] >>= fun v3' ->

      check S.contents_t "v1" v1 v1';
      check S.contents_t "v2" v2 v2';
      check S.contents_t "v3" v3 v3';

      Lwt.return_unit
    in
    run x test

  let test_merge_unrelated x () =
    run x @@ fun repo ->
    let v1 = "X1" in
    S.of_branch repo "foo" >>= fun foo ->
    S.of_branch repo "bar" >>= fun bar ->
    S.set_exn foo ~info:(infof "update foo:a") ["a"] v1 >>= fun () ->
    S.set_exn bar ~info:(infof "update bar:b") ["b"] v1 >>= fun () ->
    S.merge_into ~info:(infof "merge bar into foo") bar ~into:foo >>=
    merge_exn "merge unrelated"

  let rec write fn = function
    | 0 -> []
    | i -> (fun () -> fn i >>= Lwt_unix.yield) :: write fn (i-1)

  let perform l = Lwt_list.iter_p (fun f -> f ()) l

  let rec read fn check = function
    | 0 -> []
    | i ->
      (fun () ->
         fn i >|= fun v ->
         check i v)
      :: read fn check (i-1)

  let test_concurrent_low x () =
    let test_branches repo =
      let k = b1 in
      r1 ~repo >>= fun v ->
      let write = write (fun _i -> S.Branch.set repo k v) in
      let read =
        read
          (fun _i -> S.Branch.find repo k >|= get)
          (fun i  -> check (S.commit_t repo) (Fmt.strf "tag %d" i) v)
      in
      perform (write 1) >>= fun () ->
      perform (write 10 @ read 10 @  write 10 @ read 10)
    in
    let test_contents repo =
      kv2 ~repo >>= fun k ->
      let v = v2 in
      let t = P.Repo.contents_t repo in
      let write =
        write (fun _i ->
            with_contents repo (fun t -> P.Contents.add t v) >>= fun _ ->
            Lwt.return_unit)
      in
      let read =
        read
          (fun _i -> P.Contents.find t k >|= get)
          (fun i  -> check S.contents_t (Fmt.strf "contents %d" i) v)
      in
      perform (write 1) >>= fun () ->
      perform (write 10 @ read 10 @ write 10 @ read 10)
    in
    run x (fun repo -> Lwt.join [test_branches repo; test_contents repo])

  let test_concurrent_updates x () =
    let test_one repo =
      let k = ["a";"b";"d"] in
      let v = "X1" in
      S.master repo >>= fun t1 ->
      S.master repo >>= fun t2 ->
      let write t = write (fun i -> S.set_exn t ~info:(infof "update: one %d" i) k v) in
      let read t =
        read
          (fun _ -> S.get t k)
          (fun i -> check S.contents_t (Fmt.strf "update: one %d" i) v)
      in
      perform (write t1 10 @ write t2 10) >>= fun () ->
      perform (read t1 10)
    in
    let test_multi repo =
      let k i = ["a";"b";"c"; string_of_int i ] in
      let v i = Fmt.strf "X%d" i in
      S.master repo >>= fun t1 ->
      S.master repo >>= fun t2 ->
      let write t =
        write (fun i -> S.set_exn t ~info:(infof "update: multi %d" i) (k i) (v i))
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.strf "update: multi %d" i) (v i))
      in
      perform (write t1 10 @ write t2 10) >>= fun () ->
      perform (read t1 10)
    in
    run x (fun repo ->
        test_one   repo >>= fun () ->
        test_multi repo >>= fun () ->
        Lwt.return_unit
      )

  let test_concurrent_merges x () =
    let test repo =
      let k i = ["a";"b";"c"; string_of_int i ] in
      let v i = Fmt.strf "X%d" i in
      S.master repo >>= fun t1 ->
      S.master repo >>= fun t2 ->
      let write t n =
        write (fun i ->
            let tag = Fmt.strf "tmp-%d-%d" n i in
            S.clone ~src:t ~dst:tag >>= fun m ->
            S.set_exn m ~info:(infof "update") (k i) (v i) >>= fun () ->
            Lwt_unix.yield () >>= fun () ->
            S.merge_into ~info:(infof "update: multi %d" i) m ~into:t >>=
            merge_exn "update: multi"
          )
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.strf "update: multi %d" i) (v i))
      in
      S.set_exn t1 ~info:(infof "update") (k 0) (v 0) >>= fun () ->
      perform (write t1 1 10 @ write t2 2 10) >>= fun () ->
      perform (read t1 10)
    in
    run x test

  let pp_write_error = Irmin.Type.pp S.write_error_t
  let tree_t = testable S.tree_t

  let test_with_tree x () =
    let test repo =
      S.master repo >>= fun t ->
      let update ?retries key strategy r w =
        S.with_tree t ?retries ~info:(infof "with-tree") ~strategy key
          (fun _ ->
             Lwt_mvar.take r >|= fun v ->
             Some (`Contents (v, S.Metadata.default))
          ) >>= Lwt_mvar.put w
      in
      let check_ok = function
        | Ok ()   -> ()
        | Error e -> Alcotest.failf "%a" pp_write_error e
      in
      let check_test e = function
        | Error (`Test_was e') ->
          Alcotest.(check (option tree_t)) "test-was" e e'
        | Ok ()   -> Alcotest.fail "error expected"
        | Error e ->
          Alcotest.failf "an other error was expected: %a" pp_write_error e
      in
      let check_conflict = function
        | Error (`Conflict _) -> ()
        | Ok () -> Alcotest.fail "error expected"
        | Error e ->
          Alcotest.failf "an other error was expected: %a" pp_write_error e
      in
      let set () =
        let rx = Lwt_mvar.create_empty () in
        let wx = Lwt_mvar.create_empty () in
        let ry = Lwt_mvar.create_empty () in
        let wy = Lwt_mvar.create_empty () in
        S.set_exn t ~info:(infof "init") ["a"] "0" >>= fun () ->
        Lwt.join [
          update ["a"] ~retries:0 `Set rx wx;
          update ["a"] ~retries:0 `Set ry wy;
          (Lwt_mvar.put rx "1" >>= fun () ->
           Lwt_mvar.take wx >|= check_ok >>= fun () ->
           S.get t ["a"] >>= fun a ->
           Alcotest.(check string) "set x" "1" a;

           Lwt_mvar.put ry "2" >>= fun () ->
           Lwt_mvar.take wy >|= check_ok >>= fun () ->
           S.get t ["a"] >|= fun a ->
           Alcotest.(check string) "set y" "2" a;)
        ]
      in
      let test_and_set () =
        let rx = Lwt_mvar.create_empty () in
        let wx = Lwt_mvar.create_empty () in
        let ry = Lwt_mvar.create_empty () in
        let wy = Lwt_mvar.create_empty () in
        let rz = Lwt_mvar.create_empty () in
        let wz = Lwt_mvar.create_empty () in
        S.set_exn t ~info:(infof "init") ["a"] "0" >>= fun () ->
        Lwt.join [
          update ["a"] ~retries:0 `Test_and_set rx wx;
          update ["a"] ~retries:0 `Test_and_set ry wy;
          update ["a"] ~retries:1 `Test_and_set rz wz;
          (Lwt_mvar.put rx "1" >>= fun () ->
           Lwt_mvar.take wx >|= check_ok >>= fun () ->
           S.get t ["a"] >>= fun a ->
           Alcotest.(check string) "test-and-set x" "1" a;

           Lwt_mvar.put ry "2" >>= fun () ->
           Lwt_mvar.take wy >>= fun e ->
           check_test (Some (`Contents ("1", S.Metadata.default))) e;
           S.get t ["a"] >>= fun a ->
           Alcotest.(check string) "test-and-set y" "1" a;

           Lwt_mvar.put rz "3" >>= fun () ->
           (* there's a conflict, the transaction is restarted so need to feed a
              new value *)
           Lwt_mvar.put rz "4" >>= fun () ->
           Lwt_mvar.take wz >|= check_ok >>= fun () ->
           S.get t ["a"] >|= fun a ->
           Alcotest.(check string) "test-and-set z" "4" a;
          )]
      in
      let merge () =
        let rx = Lwt_mvar.create_empty () in
        let wx = Lwt_mvar.create_empty () in
        let ry = Lwt_mvar.create_empty () in
        let wy = Lwt_mvar.create_empty () in
        let rz = Lwt_mvar.create_empty () in
        let wz = Lwt_mvar.create_empty () in
        S.set_exn t ~info:(infof "init") ["a"] "0" >>= fun () ->
        Lwt.join [
          update ["a"] ~retries:0 `Merge rx wx;
          update ["a"] ~retries:0 `Merge ry wy;
          update ["a"] ~retries:1 `Merge rz wz;
          (Lwt_mvar.put rx "1" >>= fun () ->
           Lwt_mvar.take wx >|= check_ok >>= fun () ->
           S.get t ["a"] >>= fun a ->
           Alcotest.(check string) "merge x" "1" a;
           Lwt_mvar.put ry "2" >>= fun () ->
           Lwt_mvar.take wy >|= check_conflict >>= fun () ->
           S.get t ["a"] >>= fun a ->
           Alcotest.(check string) "merge y" a "1";

           Lwt_mvar.put rz "3" >>= fun () ->
           (* there's a conflict, the transaction is restarted so need to feed a
              new value *)
           Lwt_mvar.put rz "4" >>= fun () ->
           Lwt_mvar.take wz >|= check_ok >>= fun () ->
           S.get t ["a"] >|= fun a ->
           Alcotest.(check string) "merge z" a "4";
          )]
      in
      set () >>= test_and_set >>= merge
    in
    run x test

  let test_concurrent_head_updates x () =
    let test repo =
      let k i = ["a";"b";"c"; string_of_int i ] in
      let v i = Fmt.strf "X%d" i in
      S.master repo >>= fun t1 ->
      S.master repo >>= fun t2 ->
      let retry d fn =
        let rec aux i =
          fn () >>= function
          | true  -> Log.debug (fun f -> f "%d: ok!" d); Lwt.return_unit
          | false ->
            Log.debug (fun f -> f "%d: conflict, retrying (%d)." d i);
            aux (i+1)
        in
        aux 1
      in
      let write t n =
        write (fun i -> retry i (fun () ->
            S.Head.find t >>= fun test ->
            let tag = Fmt.strf "tmp-%d-%d" n i in
            S.clone ~src:t ~dst:tag >>= fun m ->
            S.set_exn m ~info:(infof "update") (k i) (v i) >>= fun () ->
            S.Head.find m >>= fun set ->
            Lwt_unix.yield () >>= fun () ->
            S.Head.test_and_set t ~test ~set
          ))
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.strf "update: multi %d" i) (v i))
      in
      S.set_exn t1 ~info:(infof "update") (k 0) (v 0) >>= fun () ->
      perform (write t1 1 5 @ write t2 2 5) >>= fun () ->
      perform (read t1 5)
    in
    run x test

end

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [
    "Basic operations on contents"    , speed, T.test_contents x;
    "Basic operations on nodes"       , speed, T.test_nodes x;
    "Basic operations on commits"     , speed, T.test_commits x;
    "Basic operations on branches"    , speed, T.test_branches x;
    "Watch callbacks and exceptions"  , speed, T.test_watch_exn x;
    "Basic operations on watches"     , speed, T.test_watches x;
    "Basic merge operations"          , speed, T.test_simple_merges x;
    "Basic operations on slices"      , speed, T.test_slice x;
    "Complex histories"               , speed, T.test_history x;
    "Empty stores"                    , speed, T.test_empty x;
    "Private node manipulation"       , speed, T.test_private_nodes x;
    "High-level store operations"     , speed, T.test_stores x;
    "High-level operations on trees"  , speed, T.test_trees x;
    "High-level store synchronisation", speed, T.test_sync x;
    "High-level store merges"         , speed, T.test_merge x;
    "Unrelated merges"                , speed, T.test_merge_unrelated x;
    "Low-level concurrency"           , speed, T.test_concurrent_low x;
    "Concurrent updates"              , speed, T.test_concurrent_updates x;
    "with_tree strategies"            , speed, T.test_with_tree x;
    "Concurrent head updates"         , speed, T.test_concurrent_head_updates x;
    "Concurrent merges"               , speed, T.test_concurrent_merges x;
  ]

let run name ~misc tl =
  let tl = List.map suite tl in
  Alcotest.run name (tl @ misc)
