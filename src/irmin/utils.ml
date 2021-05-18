let size_of t = Repr.unstage (Repr.size_of t)
let to_bin_string t = Repr.(unstage (to_bin_string t))
let pre_hash t = Repr.(unstage (pre_hash t))

let old_pre_hash t =
  let to_bin_string = to_bin_string t in
  let size_of_v v =
    match size_of t v with
    | Some n -> n
    | None -> String.length (to_bin_string v)
  in
  let pre_hash = pre_hash t in
  fun v k ->
    let size = size_of_v v in
    let buf = Bytes.create size in
    let off = pre_hash v buf 0 in
    k (Bytes.sub_string buf 0 off)
