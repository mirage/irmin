type t = Commit | Contents | Inode | Node [@@deriving irmin]

val to_magic : t -> char
val pp : t Fmt.t
