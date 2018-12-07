type error = [`Msg of string]

module Query: sig
  val get: string
  val get_tree: string
  val set: string
  val set_tree: string
  val update_tree: string
  val remove: string
  val merge: string
  val push: string
  val pull: string
  val clone: string
  val revert: string
  val lca: string
  val branch_info: string
  val commit_info: string
  val branches: string

  val generate_json: unit -> string
end

module type CLIENT = sig
  type t

  val post : t -> string -> string Lwt.t
end

module type S = sig
  type t

  module Store : Irmin.S

  type commit = {
    hash: Store.Hash.t;
    info: Irmin.Info.t;
    parents: Store.Hash.t list;
  }

  val execute :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> string Lwt.t

  val execute_json :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> (Irmin.Contents.json, error) result Lwt.t

  val branches : t -> (string list, error) result Lwt.t

  val set :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.Contents.t
    -> (Store.Hash.t, error) result Lwt.t

  val set_all :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.Contents.t
    -> Store.Metadata.t
    -> (Store.Hash.t, error) result Lwt.t

  val set_tree :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.tree
    -> (Store.Hash.t, error) result Lwt.t

  val update_tree :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.tree
    -> (Store.Hash.t, error) result Lwt.t

  val remove :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> (Store.Hash.t, error) result Lwt.t

  val merge :
    t
    -> ?author:string
    -> ?message:string
    -> ?into:Store.branch
    -> from:Store.branch
    -> (Store.Hash.t, error) result Lwt.t

  val get :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.Contents.t, error) result Lwt.t

  val get_all :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.Contents.t * Store.Metadata.t, error) result Lwt.t

  val get_tree :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.tree, error) result Lwt.t

  val push :
    t -> ?branch:Store.branch -> string -> (bool, error) result Lwt.t

  val pull :
    t -> ?author:string -> ?message:string -> ?branch:Store.branch -> string -> (Store.Hash.t, error) result Lwt.t

  val clone :
    t -> ?branch:Store.branch -> string -> (Store.Hash.t, error) result Lwt.t

  val revert :
    t -> ?branch:Store.branch -> Store.Hash.t -> (bool, error) result Lwt.t

  val lca:
    t -> ?branch:Store.branch -> Store.Hash.t -> (commit, error) result Lwt.t

  val commit_info :
    t -> Store.Hash.t -> (commit, error) result Lwt.t

  val branch_info :
    t -> Store.branch -> (commit, error) result Lwt.t

end

module Make
    (Client : CLIENT)
    (Store : Irmin.S) :
  S
  with type t = Client.t
   and module Store = Store
