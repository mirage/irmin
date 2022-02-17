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
        try
          let hash = Option.map Irmin_unix.Resolver.Hash.find hash in
          let c : config =
            Irmin_unix.Resolver.load_config ~store:"pack" ?hash ?contents ()
          in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_tezos"
      (void @-> returning config)
      (fun () ->
        try
          let c : config = Irmin_unix.Resolver.load_config ~store:"tezos" () in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_git"
      (string_opt @-> returning config)
      (fun contents ->
        try
          let c = Irmin_unix.Resolver.load_config ~store:"git" ?contents () in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_git_mem"
      (string_opt @-> returning config)
      (fun contents ->
        try
          let c =
            Irmin_unix.Resolver.load_config ~store:"git-mem" ?contents ()
          in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_fs"
      (string_opt @-> string_opt @-> returning config)
      (fun hash contents ->
        try
          let hash = Option.map Irmin_unix.Resolver.Hash.find hash in
          let c =
            Irmin_unix.Resolver.load_config ~store:"irf" ?hash ?contents ()
          in
          Root.create_config c
        with _ -> null config)

  let () =
    fn "config_mem"
      (string_opt @-> string_opt @-> returning config)
      (fun hash contents ->
        try
          let hash = Option.map Irmin_unix.Resolver.Hash.find hash in
          let c =
            Irmin_unix.Resolver.load_config ~store:"mem" ?hash ?contents ()
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
          let (module S) = Irmin_unix.Resolver.Store.generic_keyed s in
          let k = find_config_key config key in
          let ok, config =
            match k with
            | None -> (false, config)
            | Some (Irmin.Backend.Conf.K k) ->
                let t : a Irmin.Type.t = Root.get_ty ty in
                if type_name t <> type_name (Irmin.Backend.Conf.ty k) then
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
          let (module S) = Irmin_unix.Resolver.Store.generic_keyed s in
          let k = find_config_key config "root" in
          let ok, config =
            match k with
            | None -> (false, config)
            | Some (Irmin.Backend.Conf.K k) ->
                let path =
                  Irmin.Type.of_string (Irmin.Backend.Conf.ty k) path
                  |> Result.get_ok
                in
                (true, Irmin.Backend.Conf.add config k path)
          in
          Root.set_config c (s, config);
          ok
        with _ -> false)
end
