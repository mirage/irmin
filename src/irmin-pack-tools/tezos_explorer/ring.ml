type 'a t = { n : int; mutable i : int; a : 'a option array }

let make n = { n; i = 0; a = Array.make n None }

let add r x =
  Array.set r.a r.i (Some x);
  r.i <- (r.i + 1) mod r.n

let unsafe_get r i = r.a.((r.i - i + r.n) mod r.n)
let get r i = if i <= r.n then unsafe_get r i else None
let to_list r = List.init r.n (fun i -> get r (i + 1))
