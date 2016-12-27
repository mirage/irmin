#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let git = Conf.with_pkg "git"
let http = Conf.with_pkg "http"
let unix = Conf.with_pkg "unix"
let mirage = Conf.with_pkg "mirage"

let opam =
  let nolint = [
    "tc";"mirage-tc";
    "git-unix"; "git.unix";
    "mirage-git"; "git.mirage";
  ] in
  Pkg.opam_file ~lint_deps_excluding:(Some nolint) "opam"

let () =
  Pkg.describe ~opams:[opam] "irmin" @@ fun c ->
  let git = Conf.value c git in
  let http = Conf.value c http in
  let unix = Conf.value c unix in
  let mirage = Conf.value c mirage in
  let tool = unix && http && git in
  let example x = Pkg.test ~cond:tool ~run:false ("examples/" ^ x) in
  Ok [
    Pkg.mllib ~api:["Irmin"] "src/irmin.mllib";
    Pkg.mllib "src/mem/irmin-mem.mllib";
    Pkg.mllib "src/fs/irmin-fs.mllib";
    Pkg.mllib ~cond:git    "src/git/irmin-git.mllib";
    Pkg.mllib ~cond:mirage "src/mirage/irmin-mirage.mllib";
    Pkg.mllib ~cond:http   "src/http/irmin-http.mllib";
    Pkg.mllib ~cond:unix   "src/unix/irmin-unix.mllib";
    Pkg.bin   ~cond:unix   "bin/main" ~dst:"irmin";
    Pkg.test  ~cond:tool ~dir:"test" "test/test" ~args:Cmd.(v "-e" % "-q");
    example "deploy";
    example "custom_merge";
    example "process";
    example "views";
    example "sync";
    example "irmin_git_store";
  ]
