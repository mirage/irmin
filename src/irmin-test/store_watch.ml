(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
open Common

module type Sleep = sig
  val sleep : float -> unit Lwt.t
end

module Make (Log : Logs.LOG) (Zzz : Sleep) (S : Generic_key) = struct
  include Common.Make_helpers (S)

  let sleep ?(sleep_t = 0.01) () =
    let sleep_t = min sleep_t 1. in
    Lwt.pause () >>= fun () -> Zzz.sleep sleep_t

  let now_s () = Mtime.span_to_s (Mtime_clock.elapsed ())

  (* Re-apply [f] at intervals of [sleep_t] while [f] raises exceptions and
     [while_ ()] holds. *)
  let retry ?(timeout = 15.) ?(sleep_t = 0.) ~while_ fn =
    let sleep_t = max sleep_t 0.001 in
    let t = now_s () in
    let str i = Fmt.str "%d, %.3fs" i (now_s () -. t) in
    let rec aux i =
      if now_s () -. t > timeout || not (while_ ()) then fn (str i);
      try
        fn (str i);
        Lwt.return_unit
      with ex ->
        [%log.debug "retry ex: %s" (Printexc.to_string ex)];
        let sleep_t = sleep_t *. (1. +. (float i ** 2.)) in
        sleep ~sleep_t () >>= fun () ->
        [%log.debug "Test.retry %s" (str i)];
        aux (i + 1)
    in
    aux 0

  let test_watch_exn x () =
    let test repo =
      let* t = S.main repo in
      let* h = S.Head.find t in
      let key = [ "a" ] in
      let v1 = "bar" in
      let v2 = "foo" in
      let r = ref 0 in
      let eq = Irmin.Type.(unstage (equal (Irmin.Diff.t (S.commit_t repo)))) in
      let old_head = ref h in
      let check x =
        let+ h2 = S.Head.get t in
        match !old_head with
        | None -> if eq (`Added h2) x then incr r
        | Some h -> if eq (`Updated (h, h2)) x then incr r
      in
      let* u =
        S.watch ?init:h t (fun v -> check v >|= fun () -> failwith "test")
      in
      let* v =
        S.watch ?init:h t (fun v -> check v >>= fun () -> Lwt.fail_with "test")
      in
      let* w = S.watch ?init:h t (fun v -> check v) in
      S.set_exn t ~info:(infof "update") key v1 >>= fun () ->
      let* () =
        retry
          ~while_:(fun () -> !r < 3)
          (fun n -> Alcotest.(check int) ("watch 1 " ^ n) 3 !r)
      in
      let* h = S.Head.find t in
      old_head := h;
      S.set_exn t ~info:(infof "update") key v2 >>= fun () ->
      let* () =
        retry
          ~while_:(fun () -> !r < 6)
          (fun n -> Alcotest.(check int) ("watch 2 " ^ n) 6 !r)
      in
      S.unwatch u >>= fun () ->
      S.unwatch v >>= fun () ->
      S.unwatch w >>= fun () ->
      let* h = S.Head.get t in
      old_head := Some h;
      let* u =
        S.watch_key ~init:h t key (fun _ ->
            incr r;
            failwith "test")
      in
      let* v =
        S.watch_key ~init:h t key (fun _ ->
            incr r;
            Lwt.fail_with "test")
      in
      let* w =
        S.watch_key ~init:h t key (fun _ ->
            incr r;
            Lwt.return_unit)
      in
      S.set_exn t ~info:(infof "update") key v1 >>= fun () ->
      let* () =
        retry
          ~while_:(fun () -> !r < 9)
          (fun n -> Alcotest.(check int) ("watch 3 " ^ n) 9 !r)
      in
      S.set_exn t ~info:(infof "update") key v2 >>= fun () ->
      let* () =
        retry
          ~while_:(fun () -> !r < 12)
          (fun n -> Alcotest.(check int) ("watch 4 " ^ n) 12 !r)
      in
      S.unwatch u >>= fun () ->
      S.unwatch v >>= fun () ->
      S.unwatch w >>= fun () ->
      Alcotest.(check unit) "ok!" () ();
      B.Repo.close repo
    in
    run x test

  let test_watches x () =
    let pp_w ppf (p, w) = Fmt.pf ppf "%d/%d" p w in
    let pp_s ppf = function
      | None -> Fmt.string ppf "*"
      | Some w -> pp_w ppf (w ())
    in
    let check_workers msg p w =
      match x.stats with
      | None -> Lwt.return_unit
      | Some stats ->
          retry
            ~while_:(fun _ -> true)
            (fun s ->
              let got = stats () in
              let exp = (p, w) in
              let msg = Fmt.str "workers: %s %a (%s)" msg pp_w got s in
              if got = exp then line msg
              else (
                [%log.debug
                  "check-worker: expected %a, got %a" pp_w exp pp_w got];
                Alcotest.failf "%s: %a / %a" msg pp_w got pp_w exp))
    in
    let module State = struct
      type t = {
        mutable adds : int;
        mutable updates : int;
        mutable removes : int;
      }

      let pp ppf { adds; updates; removes } =
        Fmt.pf ppf "{ adds=%d; updates=%d; removes=%d }" adds updates removes

      let empty () = { adds = 0; updates = 0; removes = 0 }

      let add t =
        [%log.debug "add %a" pp t];
        t.adds <- t.adds + 1

      let update t =
        [%log.debug "update %a" pp t];
        t.updates <- t.updates + 1

      let remove t =
        [%log.debug "remove %a" pp t];
        t.removes <- t.removes + 1

      let pretty ppf t = Fmt.pf ppf "%d/%d/%d" t.adds t.updates t.removes
      let xpp ppf (a, u, r) = Fmt.pf ppf "%d/%d/%d" a u r
      let xadd (a, u, r) = (a + 1, u, r)
      let xupdate (a, u, r) = (a, u + 1, r)
      let xremove (a, u, r) = (a, u, r + 1)

      let less_than a b =
        a.adds <= b.adds
        && a.updates <= b.updates
        && a.removes <= b.removes
        && not (a = b)

      let check ?sleep_t msg (p, w) (a_adds, a_updates, a_removes) b =
        let a = { adds = a_adds; updates = a_updates; removes = a_removes } in
        check_workers msg p w >>= fun () ->
        retry ?sleep_t
          ~while_:(fun () -> less_than b a (* While [b] converges toward [a] *))
          (fun s ->
            let msg = Fmt.str "state: %s (%s)" msg s in
            if a = b then line msg
            else Alcotest.failf "%s: %a / %a" msg pp a pp b)

      let process ?sleep_t t head =
        let* () =
          match sleep_t with None -> Lwt.return_unit | Some s -> Zzz.sleep s
        in
        let () =
          match head with
          | `Added _ -> add t
          | `Updated _ -> update t
          | `Removed _ -> remove t
        in
        Lwt.return_unit

      let apply msg state kind fn ?(first = false) on s n =
        let msg mode n w s =
          let kind =
            match kind with
            | `Add -> "add"
            | `Update -> "update"
            | `Remove -> "remove"
          in
          let mode =
            match mode with `Pre -> "[pre-condition]" | `Post -> ""
          in
          Fmt.str "%s %s %s %d on=%b expected=%a:%a current=%a:%a" mode msg kind
            n on xpp s pp_w w pretty state pp_s x.stats
        in
        let check mode n w s = check (msg mode n w s) w s state in
        let incr =
          match kind with
          | `Add -> xadd
          | `Update -> xupdate
          | `Remove -> xremove
        in
        let rec aux pre = function
          | 0 -> Lwt.return_unit
          | i ->
              let pre_w =
                if on then (1, if i = n && first then 0 else 1) else (0, 0)
              in
              let post_w = if on then (1, 1) else (0, 0) in
              let post = if on then incr pre else pre in
              (* check pre-condition *)
              check `Pre (n - i) pre_w pre >>= fun () ->
              [%log.debug "[waiting for] %s" (msg `Post (n - i) post_w post)];
              fn (n - i) >>= fun () ->
              (* check post-condition *)
              check `Post (n - i) post_w post >>= fun () -> aux post (i - 1)
        in
        aux s n
    end in
    let test repo1 =
      let* t1 = S.main repo1 in
      let* repo = S.Repo.v x.config in
      let* t2 = S.main repo in
      [%log.debug "WATCH"];
      let state = State.empty () in
      let sleep_t = 0.02 in
      let process = State.process ~sleep_t state in
      let stops_0 = ref [] in
      let stops_1 = ref [] in
      let rec watch = function
        | 0 -> Lwt.return_unit
        | n ->
            let t = if n mod 2 = 0 then t1 else t2 in
            let* s = S.watch t process in
            if n mod 2 = 0 then stops_0 := s :: !stops_0
            else stops_1 := s :: !stops_1;
            watch (n - 1)
      in
      let v1 = "X1" in
      let v2 = "X2" in
      S.set_exn t1 ~info:(infof "update") [ "a"; "b" ] v1 >>= fun () ->
      S.Branch.remove repo1 S.Branch.main >>= fun () ->
      State.check "init" (0, 0) (0, 0, 0) state >>= fun () ->
      watch 100 >>= fun () ->
      State.check "watches on" (1, 0) (0, 0, 0) state >>= fun () ->
      S.set_exn t1 ~info:(infof "update") [ "a"; "b" ] v1 >>= fun () ->
      State.check "watches adds" (1, 1) (100, 0, 0) state >>= fun () ->
      S.set_exn t2 ~info:(infof "update") [ "a"; "c" ] v1 >>= fun () ->
      State.check "watches updates" (1, 1) (100, 100, 0) state >>= fun () ->
      S.Branch.remove repo S.Branch.main >>= fun () ->
      State.check "watches removes" (1, 1) (100, 100, 100) state >>= fun () ->
      Lwt_list.iter_s (fun f -> S.unwatch f) !stops_0 >>= fun () ->
      S.set_exn t2 ~info:(infof "update") [ "a" ] v1 >>= fun () ->
      State.check "watches half off" (1, 1) (150, 100, 100) state >>= fun () ->
      Lwt_list.iter_s (fun f -> S.unwatch f) !stops_1 >>= fun () ->
      S.set_exn t1 ~info:(infof "update") [ "a" ] v2 >>= fun () ->
      State.check "watches off" (0, 0) (150, 100, 100) state >>= fun () ->
      [%log.debug "WATCH-ALL"];
      let state = State.empty () in
      let* head = r1 ~repo in
      let add =
        State.apply "branch-watch-all" state `Add (fun n ->
            let tag = Fmt.str "t%d" n in
            S.Branch.set repo tag head)
      in
      let remove =
        State.apply "branch-watch-all" state `Remove (fun n ->
            let tag = Fmt.str "t%d" n in
            S.Branch.remove repo tag)
      in
      let* main = S.Branch.get repo "main" in
      let* u =
        S.Branch.watch_all
          ~init:[ ("main", main) ]
          repo
          (fun _ -> State.process state)
      in
      add true (0, 0, 0) 10 ~first:true >>= fun () ->
      remove true (10, 0, 0) 5 >>= fun () ->
      S.unwatch u >>= fun () ->
      add false (10, 0, 5) 4 >>= fun () ->
      remove false (10, 0, 5) 4 >>= fun () ->
      [%log.debug "WATCH-KEY"];
      let state = State.empty () in
      let path1 = [ "a"; "b"; "c" ] in
      let path2 = [ "a"; "d" ] in
      let path3 = [ "a"; "b"; "d" ] in
      let add =
        State.apply "branch-key" state `Add (fun _ ->
            let v = "" in
            S.set_exn t1 ~info:(infof "set1") path1 v >>= fun () ->
            S.set_exn t1 ~info:(infof "set2") path2 v >>= fun () ->
            S.set_exn t1 ~info:(infof "set3") path3 v >>= fun () ->
            Lwt.return_unit)
      in
      let update =
        State.apply "branch-key" state `Update (fun n ->
            let v = string_of_int n in
            S.set_exn t2 ~info:(infof "update1") path1 v >>= fun () ->
            S.set_exn t2 ~info:(infof "update2") path2 v >>= fun () ->
            S.set_exn t2 ~info:(infof "update3") path3 v >>= fun () ->
            Lwt.return_unit)
      in
      let remove =
        State.apply "branch-key" state `Remove (fun _ ->
            S.remove_exn t1 ~info:(infof "remove1") path1 >>= fun () ->
            S.remove_exn t1 ~info:(infof "remove2") path2 >>= fun () ->
            S.remove_exn t1 ~info:(infof "remove3") path3 >>= fun () ->
            Lwt.return_unit)
      in
      S.remove_exn t1 ~info:(infof "clean") [] >>= fun () ->
      let* init = S.Head.get t1 in
      let* u = S.watch_key t1 ~init path1 (State.process state) in
      add true (0, 0, 0) 1 ~first:true >>= fun () ->
      update true (1, 0, 0) 10 >>= fun () ->
      remove true (1, 10, 0) 1 >>= fun () ->
      S.unwatch u >>= fun () ->
      add false (1, 10, 1) 3 >>= fun () ->
      update false (1, 10, 1) 5 >>= fun () ->
      remove false (1, 10, 1) 4 >>= fun () ->
      [%log.debug "WATCH-MORE"];
      let state = State.empty () in
      let update =
        State.apply "watch-more" state `Update (fun n ->
            let v = string_of_int n in
            let path1 = [ "a"; "b"; "c"; string_of_int n; "1" ] in
            let path2 = [ "a"; "x"; "c"; string_of_int n; "1" ] in
            let path3 = [ "a"; "y"; "c"; string_of_int n; "1" ] in
            S.set_exn t2 ~info:(infof "update1") path1 v >>= fun () ->
            S.set_exn t2 ~info:(infof "update2") path2 v >>= fun () ->
            S.set_exn t2 ~info:(infof "update3") path3 v >>= fun () ->
            Lwt.return_unit)
      in
      S.remove_exn t1 ~info:(infof "remove") [ "a" ] >>= fun () ->
      S.set_exn t1 ~info:(infof "prepare") [ "a"; "b"; "c" ] "" >>= fun () ->
      let* h = S.Head.get t1 in
      let* u = S.watch_key t2 ~init:h [ "a"; "b" ] (State.process state) in
      update true (0, 0, 0) 10 ~first:true >>= fun () ->
      S.unwatch u >>= fun () ->
      update false (0, 10, 0) 10 >>= fun () ->
      B.Repo.close repo >>= fun () -> B.Repo.close repo1
    in
    run x test

  let tests =
    (* [test_watches] has been disabled for being flaky.
        TODO: work out why, fix it, and re-enable it.
        See https://github.com/mirage/irmin/issues/1447. *)
    let _ = ("Basic operations", test_watches) in
    [ ("Callbacks and exceptions", test_watch_exn) ]
end
