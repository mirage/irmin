let toplevel ~root name = Filename.(concat root name)

let pack = toplevel "store.pack"

let branch = toplevel "store.branches"

let dict = toplevel "store.dict"

let stores ~root = [ pack ~root; branch ~root; dict ~root ]

let flip = toplevel "flip"
