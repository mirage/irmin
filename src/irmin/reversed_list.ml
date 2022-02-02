type 'a t = 'a list = [] | ( :: ) of 'a * 'a t

let t a_t = Type.list a_t
let rev = List.rev
