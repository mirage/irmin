(** This module holds an ocaml value to mirror the environment variable
    [IRMIN_LAYERS_DEBUG] *)

(** [irmin_layers_debug] is the value of the environment variable [IRMIN_LAYERS_DEBUG]; if
    set, this will aid debugging by e.g. not deleting temporary worker files during
    execution *)
let irmin_layers_debug = Sys.getenv_opt "IRMIN_LAYERS_DEBUG"

let debug_mode = irmin_layers_debug <> None

