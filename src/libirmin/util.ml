let error_msg : string option ref = ref None

module Make (I : Cstubs_inverted.INTERNAL) = struct
  include Ctypes
  include Types
  include Unsigned

  let find_config_key config name =
    Irmin.Backend.Conf.Spec.find_key (Irmin.Backend.Conf.spec config) name

  let type_name x = Fmt.to_to_string Irmin.Type.pp_ty x

  let catch return f =
    try
      let () = error_msg := None in
      f ()
    with exn ->
      let () = error_msg := Some (Printexc.to_string exn) in
      return
    [@@inline]

  let catch' f = catch null f

  let free store =
    if not (is_null store) then
      (fun x -> catch () (fun () -> Ctypes.Root.release x)) store

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

  let rec run x =
    Lwt.wakeup_paused ();
    match Lwt.poll x with
    | Some x -> x
    | None ->
        let () = Lwt_engine.iter true in
        run x

  module Root = struct
    let get_repo (type a) x : a repo = Root.get x [@@inline]

    let create_repo (type a) (module S : Irmin.Generic_key.S with type repo = a)
        (r : a repo) =
      Root.create r
      [@@inline]

    let get_store (type a) x : a store = Root.get x [@@inline]

    let create_store (type a) (module S : Irmin.Generic_key.S with type t = a)
        (r : a store) =
      Root.create r
      [@@inline]

    let get_config x : config = Root.get x
    let create_config (r : config) = Root.create r
    let set_config ptr (x : config) = Root.set ptr x
    let get_ty x : 'a Irmin.Type.t = Root.get x
    let create_ty (x : 'a Irmin.Type.t) = Root.create x
    let get_value x : 'a = Root.get x
    let set_value ptr x = Root.set ptr x
    let create_value (x : 'a) = Root.create x

    let get_path (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a) x : S.path
        =
      Root.get x

    let create_path (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (r : S.path) =
      Root.create r

    let get_metadata (type a)
        (module S : Irmin.Generic_key.S with type Schema.Metadata.t = a) x :
        S.metadata =
      Root.get x

    let create_metadata (type a)
        (module S : Irmin.Generic_key.S with type Schema.Metadata.t = a)
        (r : S.metadata) =
      Root.create r

    let get_hash (type a)
        (module S : Irmin.Generic_key.S with type Schema.Hash.t = a) x : S.hash
        =
      Root.get x

    let create_hash (type a)
        (module S : Irmin.Generic_key.S with type Schema.Hash.t = a)
        (r : S.hash) =
      Root.create r

    let get_commit_key (type a)
        (module S : Irmin.Generic_key.S with type commit_key = a) x :
        S.commit_key =
      Root.get x

    let create_commit_key (type a)
        (module S : Irmin.Generic_key.S with type commit_key = a)
        (r : S.commit_key) =
      Root.create r

    let get_kinded_key (type a b c)
        (module S : Irmin.Generic_key.S
          with type node_key = a
           and type contents_key = b
           and type Schema.Metadata.t = c) x : S.Tree.kinded_key =
      Root.get x

    let create_kinded_key (type a b c)
        (module S : Irmin.Generic_key.S
          with type node_key = a
           and type contents_key = b
           and type Schema.Metadata.t = c) (r : S.Tree.kinded_key) =
      Root.create r

    let get_tree (type a) (module S : Irmin.Generic_key.S with type tree = a) x
        : S.tree =
      Root.get x

    let create_tree (type a) (module S : Irmin.Generic_key.S with type tree = a)
        (r : S.tree) =
      Root.create r

    let set_tree (type a) (module S : Irmin.Generic_key.S with type tree = a)
        ptr (r : S.tree) =
      Root.set ptr r

    let get_commit (type a)
        (module S : Irmin.Generic_key.S with type commit = a) x : S.commit =
      Root.get x

    let create_commit (type a)
        (module S : Irmin.Generic_key.S with type commit = a) (r : S.commit) =
      Root.create r

    let get_contents (type a)
        (module S : Irmin.Generic_key.S with type Schema.Contents.t = a) x :
        S.contents =
      Root.get x

    let create_contents (type a)
        (module S : Irmin.Generic_key.S with type Schema.Contents.t = a)
        (r : S.contents) =
      Root.create r

    let get_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a) x : S.info
        =
      Root.get x

    let set_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a) ptr
        (x : S.info) : unit =
      Root.set ptr x

    let create_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a)
        (r : S.info) =
      Root.create r

    let get_string x : string = Root.get x
    let set_string ptr (x : string) : unit = Root.set ptr x
    let create_string (s : string) = Root.create s

    let get_branch_list (type a)
        (module S : Irmin.Generic_key.S with type Schema.Branch.t = a) x :
        a array =
      Root.get x

    let create_branch_list (type a)
        (module S : Irmin.Generic_key.S with type Schema.Branch.t = a)
        (x : S.Branch.t list) =
      Root.create (Array.of_list x)

    let get_path_list (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a) x : a array
        =
      Root.get x

    let create_path_list (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (x : S.Path.t list) =
      Root.create (Array.of_list x)

    let get_commit_list (type a)
        (module S : Irmin.Generic_key.S with type commit = a) x : a array =
      Root.get x

    let create_commit_list (type a)
        (module S : Irmin.Generic_key.S with type commit = a) (x : a list) =
      Root.create (Array.of_list x)
  end
end
