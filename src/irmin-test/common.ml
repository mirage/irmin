(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let () = Random.self_init ()
let random_char () = char_of_int (Random.int 256)

let random_ascii () =
  let chars = "0123456789abcdefghijklmnopqrstABCDEFGHIJKLMNOPQRST-_." in
  chars.[Random.int @@ String.length chars]

let random_string n = String.init n (fun _i -> random_char ())
let long_random_string = random_string (* 1024_000 *) 10
let random_ascii_string n = String.init n (fun _i -> random_ascii ())
let long_random_ascii_string = random_ascii_string 1024_000

let merge_exn msg x =
  match x with
  | Ok x -> Lwt.return x
  | Error (`Conflict m) -> Alcotest.failf "%s: %s" msg m

open Astring

module type S =
  Irmin.S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Contents.t = string
     and type Schema.Branch.t = string

module type Layered_store =
  Irmin_layers.S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Contents.t = string
     and type Schema.Branch.t = string

module Schema (M : Irmin.Metadata.S) = struct
  module Hash = Irmin.Hash.SHA1
  module Commit = Irmin.Commit.Make (Hash)
  module Path = Irmin.Path.String_list
  module Metadata = M
  module Node = Irmin.Node.Make (Hash) (Path) (Metadata)
  module Branch = Irmin.Branch.String
  module Info = Irmin.Info.Default
  module Contents = Irmin.Contents.String
end

let store : (module Irmin.Maker) -> (module Irmin.Metadata.S) -> (module S) =
 fun (module B) (module M) ->
  let module S = B.Make (Schema (M)) in
  (module S)

let layered_store :
    (module Irmin_layers.Maker) ->
    (module Irmin.Metadata.S) ->
    (module Layered_store) =
 fun (module B) (module M) ->
  let module S = B.Make (Schema (M)) in
  (module S)

type t = {
  name : string;
  init : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  config : Irmin.config;
  store : (module S);
  layered_store : (module Layered_store) option;
  stats : (unit -> int * int) option;
}

module type Store_tests = functor (S : S) -> sig
  val tests : (string * (t -> unit -> unit)) list
end

module Make_helpers (S : S) = struct
  module P = S.Private
  module Graph = Irmin.Node.Graph (P.Node)

  let info message =
    let date = Int64.of_float 0. in
    let author = Printf.sprintf "TESTS" in
    S.Info.v ~author ~message date

  let infof fmt = Fmt.kstrf (fun str () -> info str) fmt

  let get_contents_key = function
    | `Contents key -> key
    | _ -> Alcotest.fail "expecting contents_key"

  let get_node_key = function
    | `Node key -> key
    | _ -> Alcotest.fail "expecting node_key"

  type x = int [@@deriving irmin]

  let v repo = P.Repo.contents_t repo
  let n repo = P.Repo.node_t repo
  let ct repo = P.Repo.commit_t repo
  let g repo = P.Repo.node_t repo
  let h repo = P.Repo.commit_t repo
  let b repo = P.Repo.branch_t repo
  let v1 = long_random_string
  let v2 = ""
  let with_contents repo f = P.Repo.batch repo (fun t _ _ -> f t)
  let with_node repo f = P.Repo.batch repo (fun _ t _ -> f t)
  let with_commit repo f = P.Repo.batch repo (fun _ _ t -> f t)
  let with_info repo n f = with_commit repo (fun h -> f h ~info:(info n))
  let kv1 ~repo = with_contents repo (fun t -> P.Contents.add t v1)
  let kv2 ~repo = with_contents repo (fun t -> P.Contents.add t v2)
  let normal x = `Contents (x, S.Metadata.default)
  let b1 = "foo"
  let b2 = "bar/toto"

  let n1 ~repo =
    let* kv1 = kv1 ~repo in
    with_node repo (fun t -> Graph.v t [ ("x", normal kv1) ])

  let n2 ~repo =
    let* kn1 = n1 ~repo in
    with_node repo (fun t -> Graph.v t [ ("b", `Node kn1) ])

  let n3 ~repo =
    let* kn2 = n2 ~repo in
    with_node repo (fun t -> Graph.v t [ ("a", `Node kn2) ])

  let n4 ~repo =
    let* kn1 = n1 ~repo in
    let* kv2 = kv2 ~repo in
    let* kn4 = with_node repo (fun t -> Graph.v t [ ("x", normal kv2) ]) in
    let* kn5 =
      with_node repo (fun t -> Graph.v t [ ("b", `Node kn1); ("c", `Node kn4) ])
    in
    with_node repo (fun t -> Graph.v t [ ("a", `Node kn5) ])

  let r1 ~repo =
    let* kn2 = n2 ~repo in
    S.Tree.of_key repo (`Node kn2) >>= function
    | None -> Alcotest.fail "r1"
    | Some tree ->
        S.Commit.v repo ~info:S.Info.empty ~parents:[] (tree :> S.tree)

  let r2 ~repo =
    let* kn3 = n3 ~repo in
    let* kr1 = r1 ~repo in
    S.Tree.of_key repo (`Node kn3) >>= function
    | None -> Alcotest.fail "r2"
    | Some t3 ->
        S.Commit.v repo ~info:S.Info.empty ~parents:[ S.Commit.key kr1 ]
          (t3 :> S.tree)

  let run x test =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           let* () = x.init () in
           let* repo = S.Repo.v x.config in
           test repo >>= x.clean)
         (fun exn ->
           let bt = Printexc.get_raw_backtrace () in
           x.clean () >>= fun () -> Printexc.raise_with_backtrace exn bt))
end

let ignore_srcs src =
  List.mem (Logs.Src.name src)
    [
      "git.inflater.decoder";
      "git.deflater.encoder";
      "git.encoder";
      "git.decoder";
      "git.loose";
      "git.store";
      "cohttp.lwt.io";
    ]

let reporter ?(prefix = "") () =
  let pad n x =
    if String.length x > n then x
    else x ^ String.v ~len:(n - String.length x) (fun _ -> ' ')
  in
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt
        Fmt.(styled `Magenta string)
        (pad 15 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    if ignore_srcs src then Format.ikfprintf k ppf fmt
    else with_stamp header tags k fmt
  in
  { Logs.report }

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ())

let line ppf ?color c =
  let line = String.v ~len:80 (fun _ -> c) in
  match color with
  | Some c -> Fmt.pf ppf "%a\n%!" Fmt.(styled c string) line
  | None -> Fmt.pf ppf "%s\n%!" line

let line msg =
  let line () = line Fmt.stderr ~color:`Yellow '-' in
  line ();
  Logs.info (fun f -> f "ASSERT %s" msg);
  line ()

let ( / ) = Filename.concat

let testable t =
  Alcotest.testable (Irmin.Type.pp_dump t) Irmin.Type.(unstage (equal t))

let check t = Alcotest.check (testable t)

let checks t =
  let t = Alcotest.slist (testable t) Irmin.Type.(unstage (compare t)) in
  Alcotest.check t

(* also in test/irmin-pack/common.ml *)
let check_raises_lwt msg exn (type a) (f : unit -> a Lwt.t) =
  Lwt.catch
    (fun x ->
      let* (_ : a) = f x in
      Alcotest.failf
        "Fail %s: expected function to raise %s, but it returned instead." msg
        (Printexc.to_string exn))
    (function
      | e when e = exn -> Lwt.return_unit
      | e ->
          Alcotest.failf
            "Fail %s: expected function to raise %s, but it raised %s instead."
            msg (Printexc.to_string exn) (Printexc.to_string e))

module T = Irmin.Type
