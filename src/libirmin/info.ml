module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "info_new"
      (repo @-> string_opt @-> string @-> returning info)
      (fun (type repo) repo author message ->
        with_repo' repo info
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let module Info = Irmin_unix.Info (Store.Info) in
            let info : Info.t = Info.v ?author "%s" message () in
            Root.create_info (module Store) info))

  let () =
    fn "info_update"
      (repo @-> info @-> string_opt @-> string @-> returning void)
      (fun (type repo) repo info author message ->
        with_repo repo ()
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let module Info = Irmin_unix.Info (Store.Info) in
            Root.set_info (module Store) info (Info.v ?author "%s" message ())))

  let () =
    fn "info_message"
      (repo @-> info @-> returning irmin_string)
      (fun (type repo) repo info ->
        with_repo' repo irmin_string
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let info = Root.get_info (module Store) info in
            let s = Store.Info.message info in
            Root.create_string s))

  let () =
    fn "info_author"
      (repo @-> info @-> returning irmin_string)
      (fun (type repo) repo info ->
        with_repo' repo irmin_string
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let info = Root.get_info (module Store) info in
            let s = Store.Info.author info in
            Root.create_string s))

  let () =
    fn "info_date"
      (repo @-> info @-> returning int64_t)
      (fun (type repo) repo info ->
        with_repo repo (-1L)
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let info = Root.get_info (module Store) info in
            Store.Info.date info))

  let () = fn "info_free" (info @-> returning void) free
end
