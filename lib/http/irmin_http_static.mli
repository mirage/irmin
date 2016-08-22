(** Static files. *)

val file_list: string list
(** [file_list] is the list of static files. *)

val size: string -> int64 option
(** [size f] is [f]'s size. *)

val read: string -> string option
(** [read f] is the contents of [f]. *)
