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

open Import
open Lwt.Infix
open Lwt.Syntax
include Atomic_write_intf

module Check_closed (S : Irmin.Atomic_write.S) = struct
  type t = { closed : bool ref; t : S.t }
  type key = S.key
  type value = S.value

  let check_not_closed t = if !(t.closed) then raise Irmin.Closed

  let mem t k =
    check_not_closed t;
    S.mem t.t k

  let find t k =
    check_not_closed t;
    S.find t.t k

  let set t k v =
    check_not_closed t;
    S.set t.t k v

  let test_and_set t k ~test ~set =
    check_not_closed t;
    S.test_and_set t.t k ~test ~set

  let remove t k =
    check_not_closed t;
    S.remove t.t k

  let list t =
    check_not_closed t;
    S.list t.t

  type watch = S.watch

  let watch t ?init f =
    check_not_closed t;
    S.watch t.t ?init f

  let watch_key t k ?init f =
    check_not_closed t;
    S.watch_key t.t k ?init f

  let unwatch t w =
    check_not_closed t;
    S.unwatch t.t w

  let v t = { closed = ref false; t }

  let close t =
    if !(t.closed) then ()
    else (
      t.closed := true;
      S.close t.t)

  let clear t =
    check_not_closed t;
    S.clear t.t
end

module Make (K : Key) (G : Git.S) = struct
  module Key = K
  module Val = Irmin.Hash.Make (G.Hash)
  module W = Irmin.Backend.Watch.Make (Key) (Val)

  let handle_git_err = function
    | Ok x -> x
    | Error e -> Fmt.kstr failwith "%a" G.pp_error e

  type t = {
    bare : bool;
    dot_git : Fpath.t;
    git_head : G.Hash.t Git.Reference.contents;
    t : G.t;
    w : W.t;
    m : Eio.Mutex.t;
  }

  let watches = Hashtbl.create 10

  type key = Key.t
  type value = Val.t
  type watch = W.watch * (unit -> unit)

  let branch_of_git r =
    let str = String.trim @@ Git.Reference.to_string r in
    match K.of_ref str with Ok r -> Some r | Error (`Msg _) -> None

  let git_of_branch r = Git.Reference.v (Fmt.to_to_string K.pp_ref r)
  let pp_key = Irmin.Type.pp Key.t

  let ref_read_opt t head =
    Lwt_eio.run_lwt @@ fun () ->
    (* Make a best-effort attempt to check that the reference actually
       exists before [read]-ing it, since the [Error `Reference_not_found]
       case causes a spurious warning to be logged inside [ocaml-git]. *)
    G.Ref.mem t head >>= function
    | false -> Lwt.return_none
    | true -> (
        let* r = G.Ref.read t head in
        match r with
        | Ok r -> Lwt.return_some r
        | Error (`Reference_not_found _ | `Not_found _) ->
            (* We may still hit this case due to a race condition, but it's very unlikely. *)
            Lwt.return_none
        | Error e -> Fmt.kstr Lwt.fail_with "%a" G.pp_error e)

  let mem { t; _ } r =
    Lwt_eio.run_lwt @@ fun () ->
    [%log.debug "mem %a" pp_key r];
    G.Ref.mem t (git_of_branch r)

  let find { t; _ } r =
    Lwt_eio.run_lwt @@ fun () ->
    [%log.debug "find %a" pp_key r];
    let b = git_of_branch r in
    let* exists = G.Ref.mem t b in
    if not exists then Lwt.return_none
    else
      let* k = G.Ref.resolve t b in
      match k with
      | Error (`Reference_not_found _) -> Lwt.return_none
      | Error e -> Fmt.kstr Lwt.fail_with "%a" G.pp_error e
      | Ok k -> Lwt.return_some k

  let listen_dir t =
    let ( / ) = Filename.concat in
    if G.has_global_watches then
      let dir = Fpath.(to_string @@ (t.dot_git / "refs" / "heads")) in
      let key file =
        match K.of_ref ("refs" / "heads" / file) with
        | Ok x -> Some x
        | Error (`Msg e) ->
            [%log.err "listen: file %s: %s" file e];
            None
      in
      W.listen_dir t.w dir ~key ~value:(find t)
    else fun () -> ()

  let watch_key t key ?init f =
    [%log.debug "watch_key %a" pp_key key];
    let stop = listen_dir t in
    let w = W.watch_key t.w key ?init f in
    (w, stop)

  let watch t ?init f =
    [%log.debug "watch"];
    let stop = listen_dir t in
    let w = W.watch t.w ?init f in
    (w, stop)

  let unwatch t (w, stop) =
    stop ();
    W.unwatch t.w w

  let v ?lock ~head ~bare t =
    let m = match lock with None -> Eio.Mutex.create () | Some l -> l in
    let dot_git = G.dotgit t in
    let write_head head =
      let head = Git.Reference.Ref head in
      let () =
        let r =
          if G.has_global_checkout then
            Eio.Mutex.use_rw ~protect:true m (fun () ->
                Lwt_eio.run_lwt @@ fun () ->
                G.Ref.write t Git.Reference.head head)
          else Ok ()
        in
        match r with
        | Error e -> [%log.err "Cannot create HEAD: %a" G.pp_error e]
        | Ok () -> ()
      in
      head
    in
    let git_head =
      match head with
      | Some h -> write_head h
      | None -> (
          match ref_read_opt t Git.Reference.head with
          | None -> write_head (git_of_branch K.main)
          | Some head -> head)
    in
    let w =
      try Hashtbl.find watches (G.dotgit t)
      with Not_found ->
        let w = W.v () in
        Hashtbl.add watches (G.dotgit t) w;
        w
    in
    { git_head; bare; t; w; dot_git; m }

  let list { t; _ } =
    Lwt_eio.run_lwt @@ fun () ->
    [%log.debug "list"];
    let+ refs = G.Ref.list t in
    List.fold_left
      (fun acc (r, _) ->
        match branch_of_git r with None -> acc | Some r -> r :: acc)
      [] refs

  let write_index t gr gk =
    [%log.debug "write_index"];
    if G.has_global_checkout then [%log.debug "write_index"];
    let git_head = Git.Reference.Ref gr in
    [%log.debug "write_index/if bare=%b head=%a" t.bare Git.Reference.pp gr];
    if (not t.bare) && git_head = t.git_head then (
      [%log.debug "write cache (%a)" Git.Reference.pp gr];

      (* FIXME G.write_index t.t gk *)
      let _ = gk in
      ())

  let pp_branch = Irmin.Type.pp K.t

  let set t r k =
    [%log.debug "set %a" pp_branch r];
    let gr = git_of_branch r in
    Eio.Mutex.use_rw ~protect:true t.m @@ fun () ->
    let e =
      Lwt_eio.run_lwt @@ fun () -> G.Ref.write t.t gr (Git.Reference.Uid k)
    in
    handle_git_err e;
    W.notify t.w r (Some k);
    write_index t gr k

  let remove t r =
    [%log.debug "remove %a" pp_branch r];
    Eio.Mutex.use_rw ~protect:true t.m @@ fun () ->
    let e = Lwt_eio.run_lwt @@ fun () -> G.Ref.remove t.t (git_of_branch r) in
    let () = handle_git_err e in
    W.notify t.w r None

  let eq_head_contents_opt x y =
    match (x, y) with
    | None, None -> true
    | Some x, Some y -> Git.Reference.equal_contents ~equal:G.Hash.equal x y
    | _ -> false

  let test_and_set t r ~test ~set =
    [%log.debug fun f ->
      let pp = Fmt.option ~none:(Fmt.any "<none>") (Irmin.Type.pp Val.t) in
      f "test_and_set %a: %a => %a" pp_branch r pp test pp set]
    ;
    let gr = git_of_branch r in
    let c = function None -> None | Some h -> Some (Git.Reference.Uid h) in
    let ok r =
      handle_git_err r;
      true
    in
    Eio.Mutex.use_rw ~protect:true t.m (fun () ->
        let x = ref_read_opt t.t gr in
        let b =
          if not (eq_head_contents_opt x (c test)) then false
          else
            match c set with
            | None ->
                let r = Lwt_eio.run_lwt @@ fun () -> G.Ref.remove t.t gr in
                ok r
            | Some h ->
                let r = Lwt_eio.run_lwt @@ fun () -> G.Ref.write t.t gr h in
                ok r
        in
        let () =
          if
            (* We do not protect [write_index] because it can take a long
               time and we don't want to hold the lock for too long. Would
               be safer to grab a lock, although the expanded filesystem
               is not critical for Irmin consistency (it's only a
               convenience for the user). *)
            b
          then W.notify t.w r set
        in
        let () =
          if b then match set with None -> () | Some v -> write_index t gr v
        in
        b)

  let close _ = ()

  let clear t =
    [%log.debug "clear"];
    Eio.Mutex.use_rw ~protect:true t.m @@ fun () ->
    let refs = Lwt_eio.run_lwt @@ fun () -> G.Ref.list t.t in
    List.iter
      (fun (r, _) ->
        let e = Lwt_eio.run_lwt @@ fun () -> G.Ref.remove t.t r in
        handle_git_err e;
        match branch_of_git r with Some k -> W.notify t.w k None | None -> ())
      refs
end
