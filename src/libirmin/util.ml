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
  include Ctypes
  include Types
  include Unsigned

  let find_config_key config name =
    Irmin.Backend.Conf.Spec.find_key (Irmin.Backend.Conf.spec config) name

  let type_name x = Fmt.to_to_string Irmin.Type.pp_ty x

  (* Generic free function for all rooted values *)
  let free x =
    let ptr = Ctypes.to_voidp x in
    if not (is_null ptr) then (fun x -> Ctypes.Root.release x) ptr

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

  let run_env fn =
    Eio_main.run @@ fun env ->
    Lwt_eio.with_event_loop ~clock:env#clock @@ fun () ->
    fn (env :> Irmin_cli.eio)

  let run fn = run_env (fun _ -> fn ())

  module Root = struct
    let to_voidp t x = Ctypes.coerce t (ptr void) x

    let of_voidp t x =
      if is_null x then failwith "null pointer"
      else Ctypes.coerce (ptr void) t x

    let get_repo (type a) (x : Struct.repo ptr) : a repo =
      Root.get (to_voidp repo x)
    [@@inline]

    let create_repo (type a) (module S : Irmin.Generic_key.S with type repo = a)
        (r : a repo) : Struct.repo ptr =
      Root.create r |> of_voidp repo
    [@@inline]

    let get_store (type a) (x : Struct.store ptr) : a store =
      Root.get (to_voidp store x)
    [@@inline]

    let create_store (type a) (module S : Irmin.Generic_key.S with type t = a)
        (r : a store) : Struct.store ptr =
      Root.create r |> of_voidp store
    [@@inline]

    let get_config (x : Struct.config ptr) : config =
      Root.get (to_voidp config x)

    let create_config (r : config) : Struct.config ptr =
      Root.create r |> of_voidp config

    let set_config (ptr : Struct.config ptr) (x : config) =
      Root.set (to_voidp config ptr) x

    let get_ty (x : Struct.ty ptr) : 'a Irmin.Type.t = Root.get (to_voidp ty x)

    let create_ty (x : 'a Irmin.Type.t) : Struct.ty ptr =
      Root.create x |> of_voidp ty

    let get_value (x : Struct.value ptr) : 'a = Root.get (to_voidp value x)
    let set_value (ptr : Struct.value ptr) x = Root.set (to_voidp value ptr) x

    let create_value (x : 'a) : Struct.value ptr =
      Root.create x |> of_voidp value

    let get_path (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (x : Struct.path ptr) : S.path =
      Root.get (to_voidp path x)

    let create_path (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (r : S.path) : Struct.path ptr =
      Root.create r |> of_voidp path

    let get_metadata (type a)
        (module S : Irmin.Generic_key.S with type Schema.Metadata.t = a)
        (x : Struct.metadata ptr) : S.metadata =
      Root.get (to_voidp metadata x)

    let create_metadata (type a)
        (module S : Irmin.Generic_key.S with type Schema.Metadata.t = a)
        (r : S.metadata) : Struct.metadata ptr =
      Root.create r |> of_voidp metadata

    let get_hash (type a)
        (module S : Irmin.Generic_key.S with type Schema.Hash.t = a)
        (x : Struct.hash ptr) : S.hash =
      Root.get (to_voidp hash x)

    let create_hash (type a)
        (module S : Irmin.Generic_key.S with type Schema.Hash.t = a)
        (r : S.hash) : Struct.hash ptr =
      Root.create r |> of_voidp hash

    let get_commit_key (type a)
        (module S : Irmin.Generic_key.S with type commit_key = a)
        (x : Struct.commit_key ptr) : S.commit_key =
      Root.get (to_voidp commit_key x)

    let create_commit_key (type a)
        (module S : Irmin.Generic_key.S with type commit_key = a)
        (r : S.commit_key) : Struct.commit_key ptr =
      Root.create r |> of_voidp commit_key

    let get_kinded_key (type a b c)
        (module S : Irmin.Generic_key.S
          with type node_key = a
           and type contents_key = b
           and type Schema.Metadata.t = c) (x : Struct.kinded_key ptr) :
        S.Tree.kinded_key =
      Root.get (to_voidp kinded_key x)

    let create_kinded_key (type a b c)
        (module S : Irmin.Generic_key.S
          with type node_key = a
           and type contents_key = b
           and type Schema.Metadata.t = c) (r : S.Tree.kinded_key) :
        Struct.kinded_key ptr =
      Root.create r |> of_voidp kinded_key

    let get_tree (type a) (module S : Irmin.Generic_key.S with type tree = a)
        (x : Struct.tree ptr) : S.tree =
      Root.get (to_voidp tree x)

    let create_tree (type a) (module S : Irmin.Generic_key.S with type tree = a)
        (r : S.tree) : Struct.tree ptr =
      Root.create r |> of_voidp tree

    let set_tree (type a) (module S : Irmin.Generic_key.S with type tree = a)
        (ptr : Struct.tree ptr) (r : S.tree) =
      Root.set (to_voidp tree ptr) r

    let get_commit (type a)
        (module S : Irmin.Generic_key.S with type commit = a)
        (x : Struct.commit ptr) : S.commit =
      Root.get (to_voidp commit x)

    let create_commit (type a)
        (module S : Irmin.Generic_key.S with type commit = a) (r : S.commit) :
        Struct.commit ptr =
      Root.create r |> of_voidp commit

    let get_contents (type a)
        (module S : Irmin.Generic_key.S with type Schema.Contents.t = a)
        (x : Struct.contents ptr) : S.contents =
      Root.get (to_voidp contents x)

    let create_contents (type a)
        (module S : Irmin.Generic_key.S with type Schema.Contents.t = a)
        (r : S.contents) : Struct.contents ptr =
      Root.create r |> of_voidp contents

    let get_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a)
        (x : Struct.info ptr) : S.info =
      Root.get (to_voidp info x)

    let set_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a)
        (ptr : Struct.info ptr) (x : S.info) : unit =
      Root.set (to_voidp info ptr) x

    let create_info (type a)
        (module S : Irmin.Generic_key.S with type Schema.Info.t = a)
        (r : S.info) : Struct.info ptr =
      Root.create r |> of_voidp info

    let get_string (x : Struct.irmin_string ptr) : string =
      Root.get (to_voidp irmin_string x)

    let set_string (ptr : Struct.irmin_string ptr) (x : string) : unit =
      Root.set (to_voidp irmin_string ptr) x

    let create_string (s : string) : Struct.irmin_string ptr =
      Root.create s |> of_voidp irmin_string

    let get_branch_array (type a)
        (module S : Irmin.Generic_key.S with type Schema.Branch.t = a)
        (x : Struct.branch_array ptr) : a array =
      Root.get (to_voidp branch_array x)

    let create_branch_array (type a)
        (module S : Irmin.Generic_key.S with type Schema.Branch.t = a)
        (x : S.Branch.t list) : Struct.branch_array ptr =
      Root.create (Array.of_list x) |> of_voidp branch_array

    let get_path_array (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (x : Struct.path_array ptr) : a array =
      Root.get (to_voidp path_array x)

    let create_path_array (type a)
        (module S : Irmin.Generic_key.S with type Schema.Path.t = a)
        (x : S.Path.t list) : Struct.path_array ptr =
      Root.create (Array.of_list x) |> of_voidp path_array

    let get_commit_array (type a)
        (module S : Irmin.Generic_key.S with type commit = a)
        (x : Struct.commit_array ptr) : a array =
      Root.get (to_voidp commit_array x)

    let create_commit_array (type a)
        (module S : Irmin.Generic_key.S with type commit = a) (x : a list) :
        Struct.commit_array ptr =
      Root.create (Array.of_list x) |> of_voidp commit_array

    let get_remote (x : Struct.remote ptr) : Irmin.remote =
      Root.get (to_voidp remote x)

    let create_remote (x : Irmin.remote) : Struct.remote ptr =
      Root.create x |> of_voidp remote
  end

  (* Handle errors and set error function, returns [return] if an exception is raised *)
  let with_repo (repo : Struct.repo ptr) return f =
    let repo = Root.get_repo repo in
    try
      repo.error <- None;
      f repo.repo_mod repo.repo
    with
    | Failure msg | Invalid_argument msg ->
        repo.error <- Some msg;
        return
    | exn ->
        repo.error <- Some (Printexc.to_string exn);
        return
  [@@inline]

  let null t = Ctypes.coerce (ptr void) t null

  (* Similar to [with_repo] but returns a null pointer *)
  let with_repo' (repo : Struct.repo ptr) t f = with_repo repo (null t) f

  let with_store (store : Struct.store ptr) return f =
    let store = Root.get_store store in
    let ctx = Root.get_repo (Root.of_voidp repo store.repo) in
    try
      ctx.error <- None;
      f store.store_mod store.store
    with
    | Failure msg | Invalid_argument msg ->
        ctx.error <- Some msg;
        return
    | exn ->
        ctx.error <- Some (Printexc.to_string exn);
        return
  [@@inline]

  (* Similar to [with_store] but returns a null pointer *)
  let with_store' (store : Struct.store ptr) t f = with_store store (null t) f
end
