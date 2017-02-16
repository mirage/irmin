#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let includes = function
  | "irmin"        -> ["src"]
  | "irmin-git"    -> ["src-git"]
  | "irmin-mirage" -> ["src-mirage"]
  | "irmin-http"   -> ["src-http"]
  | "irmin-unix"   -> ["src-unix"]
  | x -> failwith ("Unknown includes for package: " ^ x)

let extra_deps c = match Conf.pkg_name c with
  | "irmin"        -> []
  | "irmin-git"    -> ["irmin"]
  | "irmin-mirage" -> ["irmin"; "irmin-git"]
  | "irmin-http"   -> ["irmin"]@if Conf.build_tests c then ["irmin-git"] else []
  | "irmin-unix"   -> ["irmin-git"; "irmin-http"]
  | x -> failwith ("Unknown includes for package: " ^ x)

let build =
  let cmd c os =
    let includes = match includes (Conf.pkg_name c) with
      | [] -> Cmd.empty
      | is -> Cmd.(v "-Is" % String.concat "," is)
    in
    let extra_deps = match extra_deps c with
      | [] -> Cmd.empty
      | ed -> Cmd.(v "-package" % String.concat "," ed)
    in
    Cmd.(Pkg.build_cmd c os %% includes %% extra_deps)
  in
  let cmd c os files = OS.Cmd.run @@ Cmd.(cmd c os %% of_list files) in
  Pkg.build ~cmd ()

let metas = [
  Pkg.meta_file ~install:false "pkg/META";
  Pkg.meta_file ~install:false "pkg/META.http";
  Pkg.meta_file ~install:false "pkg/META.unix";
  Pkg.meta_file ~install:false "pkg/META.mirage";
]

let opams =
  let opam name =
    Pkg.opam_file ~lint_deps_excluding:None ~install:false name
  in
  [
    opam "irmin.opam";
    opam "irmin-git.opam";
    opam "irmin-http.opam";
    opam "irmin-unix.opam";
    opam "irmin-mirage.opam";
  ]

let example x = Pkg.test ~run:false ("examples/" ^ x)

let () =
  Pkg.describe ~build ~opams ~metas "irmin" @@ fun c ->
  match Conf.pkg_name c with
  | "irmin" ->
    Ok [
      Pkg.lib "pkg/META";
      Pkg.lib "irmin.opam" ~dst:"opam";
      Pkg.mllib ~api:["Irmin"; "Irmin_mem"; "Irmin_fs"] "src/irmin.mllib";
      Pkg.test ~dir:"_build" "test/main" ~args:Cmd.(v "-e" % "-q");
    ]
  | "irmin-git" ->
    Ok [
      Pkg.lib "pkg/META.git" ~dst:"META";
      Pkg.lib "irmin-git.opam" ~dst:"opam";
      Pkg.mllib "src-git/irmin-git.mllib";
      Pkg.test ~dir:"_build" "test/main_git" ~args:Cmd.(v "-e" % "-q");
    ]
  | "irmin-mirage" ->
    Ok [
      Pkg.lib "pkg/META.mirage" ~dst:"META";
      Pkg.lib "irmin-mirage.opam" ~dst:"opam";
      Pkg.mllib "src-mirage/irmin-mirage.mllib";
    ]
  | "irmin-http" ->
    Ok [
      Pkg.lib "pkg/META.http" ~dst:"META";
      Pkg.lib "irmin-http.opam" ~dst:"opam";
      Pkg.mllib ~api:["Irmin_http";"Irmin_http_server"]
        "src-http/irmin-http.mllib";
      Pkg.test ~dir:"_build" "test/main_http" ~args:Cmd.(v "-e" % "-q");
    ]
  | "irmin-unix" ->
    Ok [
      Pkg.lib "pkg/META.unix" ~dst:"META";
      Pkg.lib "irmin-unix.opam" ~dst:"opam";
      Pkg.mllib "src-unix/irmin-unix.mllib";
      Pkg.bin "bin/main" ~dst:"irmin";
      Pkg.test ~dir:"_build" "test/main_unix" ~args:Cmd.(v "-e" % "-q");
      example "deploy";
      example "custom_merge";
      example "process";
      example "views";
      example "sync";
      example "irmin_git_store";
    ]
  | other -> R.error_msgf "unknown package name: %s" other
