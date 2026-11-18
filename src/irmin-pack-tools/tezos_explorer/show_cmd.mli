val main :
  sw:Eio.Switch.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  string ->
  string ->
  string ->
  string ->
  unit
(** [main ~sw ~fs store_path info_last_path info_next_path index_path] runs the
    interactive explorer of the store at [store_path]. When notty was not
    installed at build time, it reports an error and exits with code 1. *)
