(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "log_level"
      (string_opt @-> returning bool)
      (fun level ->
        try
          match level with
          | None ->
              Logs.set_level None;
              true
          | Some level -> (
              Fmt_tty.setup_std_outputs ();
              Logs.set_reporter (Logs_fmt.reporter ());
              match Logs.level_of_string level with
              | Error _ -> false
              | Ok level ->
                  Logs.set_level level;
                  true)
        with _ -> false)

  let () =
    fn "config_pack"
      (string_opt @-> string_opt @-> returning config)
      (fun hash contents ->
        run_env @@ fun env ->
        try
          let hash = Option.map Irmin_cli.Resolver.Hash.find hash in
          let c : config =
            Irmin_cli.Resolver.load_config ~env ~store:"pack" ?hash ?contents ()
          in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_tezos"
      (void @-> returning config)
      (fun () ->
        run_env @@ fun env ->
        try
          let c : config =
            Irmin_cli.Resolver.load_config ~env ~store:"tezos" ()
          in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_git"
      (string_opt @-> returning config)
      (fun contents ->
        run_env @@ fun env ->
        try
          let c =
            Irmin_cli.Resolver.load_config ~env ~store:"git" ?contents ()
          in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_git_mem"
      (string_opt @-> returning config)
      (fun contents ->
        run_env @@ fun env ->
        try
          let c =
            Irmin_cli.Resolver.load_config ~env ~store:"git-mem" ?contents ()
          in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_fs"
      (string_opt @-> string_opt @-> returning config)
      (fun hash contents ->
        run_env @@ fun env ->
        try
          let hash = Option.map Irmin_cli.Resolver.Hash.find hash in
          let c =
            Irmin_cli.Resolver.load_config ~env ~store:"irf" ?hash ?contents ()
          in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_mem"
      (string_opt @-> string_opt @-> returning config)
      (fun hash contents ->
        run_env @@ fun env ->
        try
          let hash = Option.map Irmin_cli.Resolver.Hash.find hash in
          let c =
            Irmin_cli.Resolver.load_config ~env ~store:"mem" ?hash ?contents ()
          in
          Root.create_config c
        with _ -> null config)

  let () = fn "config_free" (config @-> returning void) free

  let () =
    fn "config_set"
      (config @-> string @-> ty @-> value @-> returning bool)
      (fun (type a) c key ty value ->
        try
          let (s, config) : config = Root.get_config c in
          let (module S) = Irmin_cli.Resolver.Store.generic_keyed s in
          let k = find_config_key config key in
          let ok, config =
            match k with
            | None -> (false, config)
            | Some (Irmin.Backend.Conf.K k) ->
                let t : a Irmin.Type.t = Root.get_ty ty in
                if type_name t <> Irmin.Backend.Conf.typename k then
                  (false, config)
                else
                  let value = Root.get_value value in
                  (true, Irmin.Backend.Conf.add config k value)
          in
          Root.set_config c (s, config);
          ok
        with _ -> false)

  let () =
    fn "config_set_root"
      (config @-> string @-> returning bool)
      (fun c path ->
        try
          let (s, config) : config = Root.get_config c in
          let (module S) = Irmin_cli.Resolver.Store.generic_keyed s in
          let k = find_config_key config "root" in
          let ok, config =
            match k with
            | None -> (false, config)
            | Some (Irmin.Backend.Conf.K k) ->
                let path =
                  Irmin.Backend.Conf.of_string k path |> Result.get_ok
                in
                (true, Irmin.Backend.Conf.add config k path)
          in
          Root.set_config c (s, config);
          ok
        with _ -> false)
end
