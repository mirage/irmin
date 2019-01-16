type error = [`Msg of string]

module Query: sig
  (** Generates JSON dictionary containing all default queries, this is used
      by bindings written in languages other than OCaml to remain consistent with
      the default OCaml queries. *)
  val generate_json: unit -> string
end

module type S = sig
  type t

  module Store : Irmin.S
  module Client : Cohttp_lwt.S.Client

  (** Initialize GraphQL client context. {headers} are sent along with the HTTP request and
      if provided {ctx} will be used as the request context.  *)
  val init: ?headers:Cohttp.Header.t -> ?ctx:Client.ctx -> Uri.t -> t

  (** Contains information single commits *)
  type commit = {
    hash: Store.Hash.t;
    info: Irmin.Info.t;
    parents: Store.Hash.t list;
  }

  (** {execute client ~vars ~operation query} sends a request to the specified GraphQL server
      and returns the results as a string *)
  val execute :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> string Lwt.t


  (** Similar to {execute}, but returns a JSON value result *)
  val execute_json :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> (Irmin.Contents.json, error) result Lwt.t

  (** Get a list of branch names *)
  val branches : t -> (Store.branch list, error) result Lwt.t

  (** {set client ~author ~message key value} sends a request to set {key} to {value} to the GraphQL
      server, if successful the new commit hash is returned *)
  val set :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.Contents.t
    -> (Store.Hash.t, error) result Lwt.t

  (** Similar to {set}, but metadata may also be specified *)
  val set_all :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.Contents.t
    -> Store.Metadata.t
    -> (Store.Hash.t, error) result Lwt.t

  (** Used to replace trees at once. {set_tree} will overwrite existing trees to contain only the
   * values specified by this call *)
  val set_tree :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.tree
    -> (Store.Hash.t, error) result Lwt.t

  (** Used to update a tree. {update_tree} will only add/remove the values specified by this function call *)
  val update_tree :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.tree
    -> (Store.Hash.t, error) result Lwt.t

  (** Remove an item from the store *)
  val remove :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> (Store.Hash.t, error) result Lwt.t


  (** {merge client ~author ~message ~into from} sends a request to the GraphQL server to merge {from} into {into}, by default {into} is set to the master branch *)
  val merge :
    t
    -> ?author:string
    -> ?message:string
    -> ?into:Store.branch
    -> Store.branch
    -> (Store.Hash.t, error) result Lwt.t

  (** {find client ~branch key} will return `Ok (Some _)` if there is a value associated with {key}, otherwise `Ok None`
      if there were no errors *)
  val find :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.Contents.t option, error) result Lwt.t

  (** {get} is similar to find, but will return an error if the key is not found *)
  val get :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.Contents.t, error) result Lwt.t

  (** Get the value and metadata associated with the provided key *)
  val get_all :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.Contents.t * Store.Metadata.t, error) result Lwt.t

  (** Returns the tree based at the provided key in the specified branch *)
  val get_tree :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.tree, error) result Lwt.t

  (** {push client ~branch uri} pushes {branch} to the remote server specified by {uri} *)
  val push :
    t -> ?branch:Store.branch -> string -> (bool, error) result Lwt.t

  (** {pull client ~author ~message ~branch uri} pulls from the remote specified by {uri} and creates a new commit in {branch} *)
  val pull :
    t -> ?author:string -> ?message:string -> ?branch:Store.branch -> string -> (Store.Hash.t, error) result Lwt.t

  (** Clone an existing Git repo *)
  val clone :
    t -> ?branch:Store.branch -> string -> (Store.Hash.t, error) result Lwt.t

  (** Revert the specified branch to the given commit hash *)
  val revert :
    t -> ?branch:Store.branch -> Store.Hash.t -> (bool, error) result Lwt.t

  (** Returns a list of the least common ancestors between the head of the specified branch and the provided commit hash *)
  val lca:
    t -> ?branch:Store.branch -> Store.Hash.t -> (commit list, error) result Lwt.t

  (** Get information about a specific commit *)
  val commit_info :
    t -> Store.Hash.t -> (commit, error) result Lwt.t

  (** Get information about a specific branch *)
  val branch_info :
    t -> Store.branch -> (commit, error) result Lwt.t

end

(** {Make} is used to provide a common implementation for all backends *)
module Make
    (Client : Cohttp_lwt.S.Client)
    (Store : Irmin.S) :
  S
  with module Store = Store
   and module Client = Client
