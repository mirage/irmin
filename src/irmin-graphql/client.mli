module Json: sig
  type t

  val of_string: string -> (t, [`Msg of string]) result
  val to_string: t -> string
  val find: t -> string list -> t option
  val find_exn: t -> string list -> t
end

module type CLIENT = sig
  include Cohttp_lwt.S.Client
  val ctx: unit -> ctx option
end

module type S = sig
  module Client: Cohttp_lwt.S.Client
  module Branch: Irmin.Branch.S

  type t
  val v: ?headers:Cohttp.Header.t -> ?ctx:Client.ctx -> ?branch:Branch.t -> Uri.t -> t
  val with_branch: t -> Branch.t option -> t
  val execute: t -> ?vars:(string * Json.t) list -> ?operation:string -> string -> string Lwt.t
  val execute_json: t -> ?vars:(string * Json.t) list -> ?operation:string -> string -> string list -> Json.t option Lwt.t
end

module Make_client(Client: Cohttp_lwt.S.Client)(Branch: Irmin.Branch.S)(Hash: Irmin.Hash.S):
  S with module Client = Client
     and module Branch = Branch

val config: Uri.t -> Irmin.config
module Make(Client : CLIENT): Irmin.S_MAKER
