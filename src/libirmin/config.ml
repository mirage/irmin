module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "log_level"
      (string_opt @-> returning void)
      (fun level ->
        catch () (fun () ->
            match level with
            | None -> Logs.set_level None
            | Some level -> (
                Fmt_tty.setup_std_outputs ();
                Logs.set_reporter (Logs_fmt.reporter ());
                match Logs.level_of_string level with
                | Error _ -> ()
                | Ok level -> Logs.set_level level)))

  let () =
    fn "error_msg"
      (void @-> returning irmin_string)
      (fun () ->
        match !Util.error_msg with
        | Some s -> Root.create_string s
        | None -> null irmin_string)

  let () =
    fn "error_msg_is_set"
      (void @-> returning bool)
      (fun () -> match !Util.error_msg with Some _ -> true | None -> false)

  let () =
    fn "config_pack"
      (string_opt @-> string_opt @-> returning config)
      (fun hash contents ->
        catch' config (fun () ->
            let hash = Option.map Irmin_unix.Resolver.Hash.find hash in
            let c : config =
              Irmin_unix.Resolver.load_config ~store:"pack" ?hash ?contents ()
            in
            Root.create_config c))

  let () =
    fn "config_tezos"
      (void @-> returning config)
      (fun () ->
        catch' config (fun () ->
            let c : config =
              Irmin_unix.Resolver.load_config ~store:"tezos" ()
            in
            Root.create_config c))

  let () =
    fn "config_git"
      (string_opt @-> returning config)
      (fun contents ->
        catch' config (fun () ->
            let c = Irmin_unix.Resolver.load_config ~store:"git" ?contents () in
            Root.create_config c))

  let () =
    fn "config_git_mem"
      (string_opt @-> returning config)
      (fun contents ->
        catch' config (fun () ->
            let c =
              Irmin_unix.Resolver.load_config ~store:"git-mem" ?contents ()
            in
            Root.create_config c))

  let () =
    fn "config_fs"
      (string_opt @-> string_opt @-> returning config)
      (fun hash contents ->
        catch' config (fun () ->
            let hash = Option.map Irmin_unix.Resolver.Hash.find hash in
            let c =
              Irmin_unix.Resolver.load_config ~store:"irf" ?hash ?contents ()
            in
            Root.create_config c))

  let () =
    fn "config_mem"
      (string_opt @-> string_opt @-> returning config)
      (fun hash contents ->
        catch' config (fun () ->
            let hash = Option.map Irmin_unix.Resolver.Hash.find hash in
            let c =
              Irmin_unix.Resolver.load_config ~store:"mem" ?hash ?contents ()
            in
            Root.create_config c))

  let () = fn "config_free" (config @-> returning void) free

  let () =
    fn "config_set"
      (config @-> string @-> ty @-> value @-> returning bool)
      (fun (type a) c key ty value ->
        catch false (fun () ->
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
            ok))
end
