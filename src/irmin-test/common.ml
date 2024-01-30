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
  | Ok x -> x
  | Error (`Conflict m) -> Alcotest.failf "%s: %s" msg m

open Astring

module type S =
  Irmin.S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Contents.t = string
     and type Schema.Branch.t = string

module type Generic_key =
  Irmin.Generic_key.S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Contents.t = string
     and type Schema.Branch.t = string

module Schema = struct
  module Hash = Irmin.Hash.SHA1
  module Commit = Irmin.Commit.Make (Hash)
  module Path = Irmin.Path.String_list
  module Metadata = Irmin.Metadata.None
  module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
  module Branch = Irmin.Branch.String
  module Info = Irmin.Info.Default
  module Contents = Irmin.Contents.String
end

let store : (module Irmin.Maker) -> (module Irmin.Metadata.S) -> (module S) =
 fun (module B) (module M) ->
  let module Schema = struct
    include Schema
    module Metadata = M
    module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
  end in
  let module S = B.Make (Schema) in
  (module S)

type store = S of (module S) | Generic_key of (module Generic_key)

type t = {
  name : string;
  init : config:Irmin.config -> unit;
  clean : config:Irmin.config -> unit;
  config : Irmin.config;
  store : store;
  stats : (unit -> int * int) option;
  (* Certain store implementations currently don't support implementing
     repository state from a slice, because their slice formats contain
     non-portable objects. For now, we disable the tests require this feature
     for such backends.

     TODO: fix slices to always contain portable objects, and extend
     [Store.import] to re-hydrate the keys in these slices (by tracking keys of
     added objects), then require all backends to run thee tests. *)
  import_supported : bool;
}

module Suite = struct
  type nonrec t = t

  let default_clean ~config ~store =
    Eio.Switch.run @@ fun sw ->
    let (module Store : Generic_key) =
      match store with
      | Generic_key x -> x
      | S (module S) -> (module S : Generic_key)
    in
    let repo = Store.Repo.v ~sw config in
    let branches = Store.Repo.branches repo in
    let () =
      List.map (fun br () -> Store.Branch.remove repo br) branches
      |> Eio.Fiber.all
    in
    Store.Repo.close repo

  let create ~name ?(init = fun ~config:_ -> ()) ?clean ~config ~store ?stats
      ?(import_supported = true) () =
    let store = S store in
    let clean = Option.value clean ~default:(default_clean ~store) in
    { name; init; clean; config; store; stats; import_supported }

  let create_generic_key ~name ?(init = fun ~config:_ -> ()) ?clean ~config
      ~store ?stats ?(import_supported = true) () =
    let store = Generic_key store in
    let clean = Option.value clean ~default:(default_clean ~store) in
    { name; init; clean; config; store; stats; import_supported }

  let name t = t.name
  let config t = t.config
  let store t = match t.store with S x -> Some x | Generic_key _ -> None

  let store_generic_key t =
    match t.store with
    | Generic_key x -> x
    | S (module S) -> (module S : Generic_key)

  let init t = t.init
  let clean t = t.clean
end

module type Store_tests = functor (S : Generic_key) -> sig
  val tests : (string * (Suite.t -> unit -> unit)) list
end

module Make_helpers (S : Generic_key) = struct
  module B = S.Backend
  module Graph = Irmin.Node.Graph (B.Node)

  let info message =
    let date = Int64.of_float 0. in
    let author = Printf.sprintf "TESTS" in
    S.Info.v ~author ~message date

  let infof fmt = Fmt.kstr (fun str () -> info str) fmt

  let get_contents_key = function
    | `Contents key -> key
    | _ -> Alcotest.fail "expecting contents_key"

  let get_node_key = function
    | `Node key -> key
    | _ -> Alcotest.fail "expecting node_key"

  type x = int [@@deriving irmin]

  let v repo = B.Repo.contents_t repo
  let n repo = B.Repo.node_t repo
  let ct repo = B.Repo.commit_t repo
  let g repo = B.Repo.node_t repo
  let h repo = B.Repo.commit_t repo
  let b repo = B.Repo.branch_t repo
  let v1 = long_random_string
  let v2 = ""
  let with_contents repo f = B.Repo.batch repo (fun t _ _ -> f t)
  let with_node repo f = B.Repo.batch repo (fun _ t _ -> f t)
  let with_commit repo f = B.Repo.batch repo (fun _ _ t -> f t)
  let with_info repo n f = with_commit repo (fun h -> f h ~info:(info n))
  let kv1 ~repo = with_contents repo (fun t -> B.Contents.add t v1)
  let kv2 ~repo = with_contents repo (fun t -> B.Contents.add t v2)
  let normal x = `Contents (x, S.Metadata.default)
  let b1 = "foo"
  let b2 = "bar/toto"

  let n1 ~repo =
    let kv1 = kv1 ~repo in
    with_node repo (fun t -> Graph.v t [ ("x", normal kv1) ])

  let n2 ~repo =
    let kn1 = n1 ~repo in
    with_node repo (fun t -> Graph.v t [ ("b", `Node kn1) ])

  let n3 ~repo =
    let kn2 = n2 ~repo in
    with_node repo (fun t -> Graph.v t [ ("a", `Node kn2) ])

  let n4 ~repo =
    let kn1 = n1 ~repo in
    let kv2 = kv2 ~repo in
    let kn4 = with_node repo (fun t -> Graph.v t [ ("x", normal kv2) ]) in
    let kn5 =
      with_node repo (fun t -> Graph.v t [ ("b", `Node kn1); ("c", `Node kn4) ])
    in
    with_node repo (fun t -> Graph.v t [ ("a", `Node kn5) ])

  let r1 ~repo =
    let kn2 = n2 ~repo in
    match S.Tree.of_key repo (`Node kn2) with
    | None -> Alcotest.fail "r1"
    | Some tree ->
        S.Commit.v repo ~info:S.Info.empty ~parents:[] (tree :> S.tree)

  let r2 ~repo =
    let kn3 = n3 ~repo in
    let kr1 = r1 ~repo in
    match S.Tree.of_key repo (`Node kn3) with
    | None -> Alcotest.fail "r2"
    | Some t3 ->
        S.Commit.v repo ~info:S.Info.empty
          ~parents:[ S.Commit.key kr1 ]
          (t3 :> S.tree)

  let ignore_thunk_errors f = try f () with _ -> ()

  let run (x : Suite.t) test =
    Eio.Switch.run @@ fun sw ->
    let repo_ptr = ref None in
    let config_ptr = ref None in
    try
      let module Conf = Irmin.Backend.Conf in
      let generate_random_root config =
        let id = Random.int 100 |> string_of_int in
        let root_value =
          let ( / ) = Filename.concat in
          match Conf.find_root config with
          | None -> ("_build" / "test_") ^ id
          | Some v -> v ^ "_" ^ id
        in
        let root_key = Conf.(root (spec config)) in
        Conf.add config root_key root_value
      in
      let config = generate_random_root x.config in
      config_ptr := Some config;
      let () = x.init ~config in
      let repo = S.Repo.v ~sw config in
      repo_ptr := Some repo;
      let () = test repo in
      let () =
        (* [test] might have already closed the repo. That
           [ignore_thunk_errors] shall be removed as soon as all stores
           support double closes. *)
        ignore_thunk_errors (fun () -> S.Repo.close repo)
      in
      x.clean ~config
    with exn ->
      (* [test] failed, attempt an errorless cleanup and forward the right
         backtrace to the user. *)
      let bt = Printexc.get_raw_backtrace () in
      let () =
        match !repo_ptr with
        | Some repo -> ignore_thunk_errors (fun () -> S.Repo.close repo)
        | None -> ()
      in
      let () =
        match !config_ptr with
        | Some config -> ignore_thunk_errors (fun () -> x.clean ~config)
        | None -> ()
      in
      Printexc.raise_with_backtrace exn bt
end

let filter_src src =
  not
    (List.mem ~equal:String.equal (Logs.Src.name src)
       [
         "git.inflater.decoder";
         "git.deflater.encoder";
         "git.encoder";
         "git.decoder";
         "git.loose";
         "git.store";
         "cohttp.lwt.io";
       ])

let reporter ?prefix () =
  Irmin.Export_for_backends.Logging.reporter ~filter_src ?prefix
    (module Mtime_clock)

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
  [%logs.info "ASSERT %s" msg];
  line ()

let ( / ) = Filename.concat

let testable t =
  Alcotest.testable (Irmin.Type.pp_dump t) Irmin.Type.(unstage (equal t))

let check t = Alcotest.check (testable t)

let checks t =
  let t = Alcotest.slist (testable t) Irmin.Type.(unstage (compare t)) in
  Alcotest.check t

(* also in test/irmin-pack/common.ml *)
let check_raises_lwt msg exn (type a) (f : unit -> a) =
  try
    let (_ : a) = f () in
    Alcotest.failf
      "Fail %s: expected function to raise %s, but it returned instead." msg
      (Printexc.to_string exn)
  with
  | e when e = exn -> ()
  | e ->
      Alcotest.failf
        "Fail %s: expected function to raise %s, but it raised %s instead." msg
        (Printexc.to_string exn) (Printexc.to_string e)

module T = Irmin.Type

module type Sleep = sig
  val sleep : float -> unit
end
