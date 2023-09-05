type t = { depth : int; path : string; obj : obj; current : bool }
and obj = Leaf | Commit of t option | Inode of inode
and inode = Tree of t list option | Values of t list option
