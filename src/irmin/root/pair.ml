type ('a, 'b) t = 'a * 'b

let first f (a, b) = (f a, b)

let second f (a, b) = (a, f b)
