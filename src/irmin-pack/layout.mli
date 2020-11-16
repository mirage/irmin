(** Defines the location of the IO instances within the main [irmin-pack] store
    directory. *)

type path := root:string -> string

val pack : path

val branch : path

val dict : path

val stores : root:string -> string list

val flip : path
