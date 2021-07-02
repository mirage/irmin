(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module Make (S : S) = struct
  include Common.Make_helpers (S)

  type mode = DirFile | FileDir | ModDel | DelMod | Mod | ModMod

  let pp_list =
    let pp_sep ppf () = Format.fprintf ppf "/" in
    fun ppf l ->
      Format.fprintf ppf "@[<h>%a@]"
        Format.(pp_print_list ~pp_sep (fun ppf s -> Format.fprintf ppf "%s" s))
        l

  let set l s =
    S.set_exn s
      ~info:(infof "Updating %a" pp_list l)
      l
      (Format.asprintf "%a" pp_list l)

  let conflict_t =
    Alcotest.testable
      (Irmin.Type.pp_dump Irmin.Merge.conflict_t)
      Irmin.Type.(unstage (equal Irmin.Merge.conflict_t))

  let dirfile mode repo =
    let la = [ "a" ] in
    let lab = [ "a"; "b" ] in
    let la, lab = match mode with DirFile -> (lab, la) | _ -> (la, lab) in
    (* Load the master branch *)
    let* master = S.master repo in
    (* Clone master into wip *)
    let* wip = S.clone ~src:master ~dst:"wip" in
    (* Set la in master *)
    let* () = set la master in
    (* Set lab in wip *)
    let* () = set lab wip in
    let* res =
      S.merge_into ~info:(infof "Merging master into wip") master ~into:wip
    in
    Alcotest.(check (result unit conflict_t))
      "File/Dir conflict" res
      (Error (`Conflict "Recursive merging of common ancestors: File/Directory"));
    Alcotest.(check unit) "ok!" () ();
    P.Repo.close repo

  let moddel mode repo =
    let la = [ "a" ] in
    (* Load the master branch *)
    let* master = S.master repo in
    (* Set la in master *)
    let* () = set la master in
    (* Clone master into wip *)
    let* wip = S.clone ~src:master ~dst:"wip" in
    (* Modify la in wip *)
    let* () = set la wip in
    (* Remove la in master *)
    let* _ = S.remove master ~info:(infof "Remove %a" pp_list la) la in
    let from, into =
      match mode with ModDel -> (master, wip) | _ -> (wip, master)
    in
    let* res =
      S.merge_into ~info:(infof "Merging master into wip") from ~into
    in
    Alcotest.(check (result unit conflict_t))
      "Modify/Delete conflict" res (Ok ());
    Alcotest.(check unit) "ok!" () ();
    P.Repo.close repo

  let modify mode repo =
    let la = [ "a" ] in
    (* Open the repo *)
    (* Load the master branch *)
    let* master = S.master repo in
    (* Set la in master *)
    let* () = set la master in
    (* Clone master into wip *)
    let* wip = S.clone ~src:master ~dst:"wip" in
    (* Modify la in wip *)
    let* () = set la wip in
    (* Remove la in master *)
    let* _ =
      match mode with
      | Mod -> Lwt.return ()
      | _ ->
          let* _ = set la master in
          Lwt.return ()
    in
    let* res =
      S.merge_into ~info:(infof "Merging master into wip") master ~into:wip
    in
    Alcotest.(check (result unit conflict_t))
      "Modify/Modify conflict" res (Ok ());
    Alcotest.(check unit) "ok!" () ();
    P.Repo.close repo

  let test_dirfile x () = run x (dirfile DirFile)
  let test_filedir x () = run x (dirfile FileDir)
  let test_moddel x () = run x (moddel ModDel)
  let test_delmod x () = run x (moddel DelMod)
  let test_mod x () = run x (modify Mod)
  let test_modmod x () = run x (modify ModMod)

  let tests =
    [
      ("Dirfile", test_dirfile);
      ("FileDir", test_filedir);
      ("Moddel", test_moddel);
      ("Delmod", test_delmod);
      ("Mod", test_mod);
      ("Modmod", test_modmod);
    ]
end
