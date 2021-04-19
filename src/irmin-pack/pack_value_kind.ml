open! Import

type t = Commit | Contents | Inode | Node

let to_magic = function
  | Commit -> 'C'
  | Contents -> 'B'
  | Inode -> 'I'
  | Node -> 'N'

let of_magic = function
  | 'C' -> Commit
  | 'B' -> Contents
  | 'I' -> Inode
  | 'N' -> Node
  | c -> Fmt.failwith "Kind.of_magic: unexpected magic char %C" c

let t = Irmin.Type.(map char) of_magic to_magic
let pp = Fmt.using to_magic Fmt.char
