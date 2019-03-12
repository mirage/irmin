val config: Uri.t -> Irmin.config
module Make(Client : Cohttp_lwt.S.Client): Irmin.S_MAKER

