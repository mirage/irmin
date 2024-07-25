(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

include Irmin.Export_for_backends

module Conf = struct
  let test_config ~env =
    let hash = Irmin_cli.Resolver.Hash.find "blake2b" in
    let _, cfg =
      Irmin_cli.Resolver.load_config ~env ~config_path:"test/irmin-cli/test.yml"
        ~store:"pack" ~contents:"string" ~hash ()
    in
    let spec = Irmin.Backend.Conf.spec cfg in
    let index_log_size =
      Irmin.Backend.Conf.get cfg Irmin_pack.Conf.Key.index_log_size
    in
    let fresh = Irmin.Backend.Conf.get cfg Irmin_pack.Conf.Key.fresh in
    Alcotest.(check string)
      "Spec name" "pack"
      (Irmin.Backend.Conf.Spec.name spec);
    Alcotest.(check int) "index-log-size" 1234 index_log_size;
    Alcotest.(check bool) "fresh" true fresh

  let misc ~env : unit Alcotest.test_case list =
    [ ("config", `Quick, fun () -> test_config ~env) ]
end

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let env :> Irmin_cli.eio =
    object
      method cwd = Eio.Stdenv.cwd env
      method clock = Eio.Stdenv.clock env
      method sw = sw
    end
  in
  Alcotest.run "irmin-cli" [ ("conf", Conf.misc ~env) ]
