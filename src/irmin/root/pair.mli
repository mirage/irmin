type ('a, 'b) t = 'a * 'b

val first : ('a1 -> 'a2) -> 'a1 * 'b -> 'a2 * 'b

val second : ('b1 -> 'b2) -> 'a * 'b1 -> 'a * 'b2
