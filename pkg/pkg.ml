#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

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
  Pkg.describe ~opams ~metas "irmin" @@ fun c ->
  match Conf.pkg_name c with
  | "irmin" ->
    Ok [
      Pkg.lib "pkg/META";
      Pkg.lib "irmin.opam" ~dst:"opam";
      Pkg.mllib ~api:["Irmin"; "Irmin_mem"; "Irmin_fs"] "src/irmin.mllib";
    ]
  | "irmin-git" ->
    Ok [
      Pkg.lib "pkg/META.git" ~dst:"META";
      Pkg.lib "irmin-git.opam" ~dst:"opam";
      Pkg.mllib "src-git/irmin-git.mllib";
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
      Pkg.mllib "src-http/irmin-http.mllib";
    ]
  | "irmin-unix" ->
    Ok [
      Pkg.lib "pkg/META.unix" ~dst:"META";
      Pkg.lib "irmin-unix.opam" ~dst:"opam";
      Pkg.mllib "src-unix/irmin-unix.mllib";
      Pkg.bin "bin/main" ~dst:"irmin";
      Pkg.test ~dir:"_build" "test/test" ~args:Cmd.(v "-e" % "-q");
      example "deploy";
      example "custom_merge";
      example "process";
      example "views";
      example "sync";
      example "irmin_git_store";
    ]
  | other -> R.error_msgf "unknown package name: %s" other
