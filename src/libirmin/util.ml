let error_msg : string option ref = ref None

module Make (I : Cstubs_inverted.INTERNAL) = struct
  include Ctypes
  include Types
  include Unsigned

  let find_config_key config name =
    Irmin.Backend.Conf.Spec.find_key (Irmin.Backend.Conf.spec config) name

  let type_name x = Fmt.to_to_string Irmin.Type.pp_ty x

  (* Handle errors and set error function, returns [return] if an exception is raised *)
  let catch return f =
    try
      let () = error_msg := None in
      f ()
    with exn ->
      let () = error_msg := Some (Printexc.to_string exn) in
      return
    [@@inline]

  let null t = Ctypes.coerce (ptr void) t null

  (* Similar to catch but returns a null pointer *)
  let catch' t f = catch (null t) f

  (* Generic free function for all rooted values *)
  let free x =
    let ptr = Ctypes.to_voidp x in
    if not (is_null ptr) then
      (fun x -> catch () (fun () -> Ctypes.Root.release x)) ptr

  let strlen ptr =
    if is_null ptr then 0
    else
      let rec loop i =
        if !@(ptr +@ i) = char_of_int 0 then i else loop (i + 1)
      in
      loop 0

  let get_length length s =
    let length = Int64.to_int length in
    if length < 0 then strlen s else length

  let fn name t f = I.internal ~runtime_lock:false ("irmin_" ^ name) t f

  (* Minimal executor for lwt promises *)
  let rec run x =
    Lwt.wakeup_paused ();
    match Lwt.poll x with
    | Some x -> x
    | None ->
        let () = Lwt_engine.iter true in
        run x

  module Root = struct
    let to_voidp x = Obj.magic x
    let of_voidp x = Obj.magic x

    let get_repo (type a) (x : Struct.repo ptr) : a repo = Root.get (to_voidp x)
      [@@inline]

    let create_repo (type a) (module S : Irmin.Generic_key.S with type repo = a)
        (r : a repo) : Struct.repo ptr =
      Root.create r |> of_voidp
      [@@inline]

    let get_store (type a) (x : Struct.store ptr) : a store =
      Root.get (to_voidp x)
      [@@inline]

    let create_store (type a) (module S : Irmin.Generic_key.S with type t = a)
        (r : a store) : Struct.store ptr =
      Root.create r |> of_voidp
      [@@inline]

    let get_config (x : Struct.config ptr) : config = Root.get (to_voidp x)

    let create_config (r : config) : Struct.config ptr =
      Root.create r |> of_voidp

    let set_config (ptr : Struct.config ptr) (x : config) =
      Root.set (to_voidp ptr) x

    let get_ty (x : Struct.ty ptr) : 'a Irmin.Type.t = Root.get (to_voidp x)

    let create_ty (x : 'a Irmin.Type.t) : Struct.ty ptr =
      Root.create x |> of_voidp

    let get_value (x : Struct.value ptr) : 'a = Root.get (to_voidp x)
    let set_value (ptr : Struct.value ptr) x = Root.set (to_voidp ptr) x
    let create_value (x : 'a) : Struct.value ptr = Root.create x |> of_voidp

    let get_path (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (x : Struct.path ptr) : S.path =
      Root.get (to_voidp x)

    let create_path (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (r : S.path) : Struct.path ptr =
      Root.create r |> of_voidp

    let get_metadata (type a)
        (module S : Irmin.Generic_key.S with type Schema.Metadata.t = a)
        (x : Struct.metadata ptr) : S.metadata =
      Root.get (of_voidp x)

    let create_metadata (type a)
        (module S : Irmin.Generic_key.S with type Schema.Metadata.t = a)
        (r : S.metadata) : Struct.metadata ptr =
      Root.create r |> of_voidp

    let get_hash (type a)
        (module S : Irmin.Generic_key.S with type Schema.Hash.t = a)
        (x : Struct.hash ptr) : S.hash =
      Root.get (to_voidp x)

    let create_hash (type a)
        (module S : Irmin.Generic_key.S with type Schema.Hash.t = a)
        (r : S.hash) : Struct.hash ptr =
      Root.create r |> of_voidp

    let get_commit_key (type a)
        (module S : Irmin.Generic_key.S with type commit_key = a)
        (x : Struct.commit_key ptr) : S.commit_key =
      Root.get (to_voidp x)

    let create_commit_key (type a)
        (module S : Irmin.Generic_key.S with type commit_key = a)
        (r : S.commit_key) : Struct.commit_key ptr =
      Root.create r |> of_voidp

    let get_kinded_key (type a b c)
        (module S : Irmin.Generic_key.S
          with type node_key = a
           and type contents_key = b
           and type Schema.Metadata.t = c) (x : Struct.kinded_key ptr) :
        S.Tree.kinded_key =
      Root.get (to_voidp x)

    let create_kinded_key (type a b c)
        (module S : Irmin.Generic_key.S
          with type node_key = a
           and type contents_key = b
           and type Schema.Metadata.t = c) (r : S.Tree.kinded_key) :
        Struct.kinded_key ptr =
      Root.create r |> of_voidp

    let get_tree (type a) (module S : Irmin.Generic_key.S with type tree = a)
        (x : Struct.tree ptr) : S.tree =
      Root.get (to_voidp x)

    let create_tree (type a) (module S : Irmin.Generic_key.S with type tree = a)
        (r : S.tree) : Struct.tree ptr =
      Root.create r |> of_voidp

    let set_tree (type a) (module S : Irmin.Generic_key.S with type tree = a)
        (ptr : Struct.tree ptr) (r : S.tree) =
      Root.set (to_voidp ptr) r

    let get_commit (type a)
        (module S : Irmin.Generic_key.S with type commit = a)
        (x : Struct.commit ptr) : S.commit =
      Root.get (of_voidp x)

    let create_commit (type a)
        (module S : Irmin.Generic_key.S with type commit = a) (r : S.commit) :
        Struct.commit ptr =
      Root.create r |> to_voidp

    let get_contents (type a)
        (module S : Irmin.Generic_key.S with type Schema.Contents.t = a)
        (x : Struct.contents ptr) : S.contents =
      Root.get (to_voidp x)

    let create_contents (type a)
        (module S : Irmin.Generic_key.S with type Schema.Contents.t = a)
        (r : S.contents) : Struct.contents ptr =
      Root.create r |> of_voidp

    let get_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a)
        (x : Struct.info ptr) : S.info =
      Root.get (to_voidp x)

    let set_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a)
        (ptr : Struct.info ptr) (x : S.info) : unit =
      Root.set (to_voidp ptr) x

    let create_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a)
        (r : S.info) : Struct.info ptr =
      Root.create r |> of_voidp

    let get_string (x : Struct.irmin_string ptr) : string =
      Root.get (of_voidp x)

    let set_string (ptr : Struct.irmin_string ptr) (x : string) : unit =
      Root.set (to_voidp ptr) x

    let create_string (s : string) : Struct.irmin_string ptr =
      Root.create s |> of_voidp

    let get_branch_list (type a)
        (module S : Irmin.Generic_key.S with type Schema.Branch.t = a)
        (x : Struct.branch_list ptr) : a array =
      Root.get (to_voidp x)

    let create_branch_list (type a)
        (module S : Irmin.Generic_key.S with type Schema.Branch.t = a)
        (x : S.Branch.t list) : Struct.branch_list ptr =
      Root.create (Array.of_list x) |> of_voidp

    let get_path_list (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (x : Struct.path_list ptr) : a array =
      Root.get (to_voidp x)

    let create_path_list (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (x : S.Path.t list) : Struct.path_list ptr =
      Root.create (Array.of_list x) |> of_voidp

    let get_commit_list (type a)
        (module S : Irmin.Generic_key.S with type commit = a)
        (x : Struct.commit_list ptr) : a array =
      Root.get (to_voidp x)

    let create_commit_list (type a)
        (module S : Irmin.Generic_key.S with type commit = a) (x : a list) :
        Struct.commit_list ptr =
      Root.create (Array.of_list x) |> of_voidp
  end
end
