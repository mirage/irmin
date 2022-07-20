open Lwt.Syntax
open Lwt.Infix

module Make
    (IO : Conn.IO)
    (Codec : Conn.Codec.S)
    (St : Irmin.Generic_key.S)
    (Tree : Tree.S
              with type kinded_key = St.Tree.kinded_key
               and type concrete = St.Tree.concrete) =
struct
  module Server_info = struct
    type t = { start_time : float }

    let uptime { start_time; _ } = IO.time () -. start_time
  end

  module Conn = Conn.Make (IO) (Codec)

  type context = {
    conn : Conn.t;
    config : Irmin.Backend.Conf.t;
    repo : St.Repo.t;
    mutable branch : St.branch;
    mutable store : St.t;
    trees : (int, St.tree) Hashtbl.t;
    mutable watch : St.watch option;
    mutable branch_watch : St.Backend.Branch.watch option;
  }

  module type CMD = sig
    type req
    type res

    val req_t : req Irmin.Type.t
    val res_t : res Irmin.Type.t
    val name : string

    val run :
      Conn.t -> context -> Server_info.t -> req -> res Conn.Return.t Lwt.t
  end

  let cmd (module C : CMD) = (C.name, (module C : CMD))
  let next_id = ref 0

  let incr_id () =
    let x = !next_id in
    incr next_id;
    x

  let reset_trees ctx =
    next_id := 0;
    Hashtbl.reset ctx.trees

  let resolve_tree ctx tree =
    let* id, tree =
      match tree with
      | Tree.ID x -> Lwt.return @@ (Some x, Hashtbl.find_opt ctx.trees x)
      | Key x -> St.Tree.of_key ctx.repo x >|= fun x -> (None, x)
      | Concrete x -> Lwt.return (None, Some (St.Tree.of_concrete x))
    in
    match tree with
    | Some t -> Lwt.return (id, t)
    | None -> Error.raise_error "unknown tree"
end
