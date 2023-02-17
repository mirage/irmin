type 'a t = { n : int; mutable i : int; a : 'a array }

let make n ~default = { n; i = 0; a = Array.make n default }

let add r x =
  Array.set r.a r.i x;
  r.i <- (r.i + 1) mod r.n

let get r i =
  if i <= r.n then
    let x = r.a.((r.i - i + r.n) mod r.n) in
    if x <> -1 then Some x else None
  else None

let to_list r =
  let rec f i acc l =
    if i == r.i then l @ List.rev acc
    else match l with [] -> List.rev acc | h :: t -> f (i + 1) (h :: acc) t
  in
  let l = Array.to_list r.a in
  List.rev (f 0 [] l)
