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
    Pkg.mllib ~api:["Irmin"] "lib/irmin.mllib";
    Pkg.mllib "lib/mem/irmin-mem.mllib";
    Pkg.mllib "lib/fs/irmin-fs.mllib";
    Pkg.mllib ~cond:git    "lib/git/irmin-git.mllib";
    Pkg.mllib ~cond:mirage "lib/mirage/irmin-mirage.mllib";
    Pkg.mllib ~cond:http   "lib/http/irmin-http.mllib";
    Pkg.mllib ~cond:unix   "lib/unix/irmin-unix.mllib";
    Pkg.bin   ~cond:unix   "bin/main" ~dst:"irmin";
    Pkg.test  ~cond:tool ~dir:"lib_test" "lib_test/test" ~args:Cmd.(v "-e" % "-q");
    example "deploy";
    example "custom_merge";
    example "process";
    example "views";
    example "sync";
    example "irmin_git_store";
  ]
