module type CLIENT = sig
  include Cohttp_lwt.S.Client
  val ctx: unit -> ctx option
end

val config: Uri.t -> Irmin.config
module Make(Client : CLIENT): Irmin.S_MAKER
