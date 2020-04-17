(** Default Irmin GraphQL store *)
module Store :
  Irmin.S
    with type contents = string
     and type step = string
     and type metadata = unit

type server = {
  event_loop : 'a. 'a Lwt.t;
      (** The server runtime. Cancelling this thread terminates the server. *)
  set_tree : Store.Tree.concrete -> unit Lwt.t;
      (** Set the state of the [master] branch in the underlying store. *)
}

val spawn_graphql_server : unit -> server Lwt.t
(** Initialise a GraphQL server. At most one server may be running concurrently. *)

val send_query : string -> (string, [ `Msg of string ]) result Lwt.t
(** Send a GraphQL query string to the currently running test GraphQL instance. *)
