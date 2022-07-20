val uri : Uri.t option Cmdliner.Term.t
(** Generic URI term *)

val default_uri : Uri.t
(** Default URI for command line applications *)

val config_path : string option Cmdliner.Term.t
(** Command line argument to specify configuration path *)

val codec : [ `Bin | `Json ] Cmdliner.Term.t
