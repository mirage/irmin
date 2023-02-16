val uri : Uri.t option Cmdliner.Term.t
(** Generic URI term *)

val default_uri : Uri.t
(** Default URI for command line applications *)

val config_path : string option Cmdliner.Term.t
(** Command line argument to specify configuration path *)

val codec : [ `Bin | `Json ] Cmdliner.Term.t

module Conf : sig
  module Key : sig
    val uri: Uri.t Irmin.Backend.Conf.key
  end
  
  val spec: Irmin.Backend.Conf.Spec.t
  val v: Irmin.config -> Uri.t option -> Irmin.config
end