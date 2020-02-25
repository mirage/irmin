type ('a, 'b) t = Left of 'a | Right of 'b

val map : ('a, 'b) t -> l:('a -> 'c) -> r:('b -> 'c) -> 'c

val left : 'a -> ('a, _) t

val right : 'b -> (_, 'b) t
