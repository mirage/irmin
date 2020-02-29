open Irmin_root
open Brands

type ('s, 't, 'a, 'b, 'm) t = {
  monad : 'm monad;
  review : 'b -> ('t, 'm) app;
  preview : 's -> ('a option, 'm) app;
}

type ('s, 'a, 'm) mono = ('s, 's, 'a, 'a, 'm) t

let v monad review preview = { monad; review; preview }

let review { review; _ } = review

let preview { preview; _ } = preview

let natural_compose :
    type m n a b c d e f.
    (d -> ((d, n) app, m) app) ->
    ((c, n) app -> (c, m) app) ->
    (a, b, (c, n) app, (d, n) app, m) t ->
    (c, d, (e, n) app, (f, n) app, m) t ->
    (a, b, (e, n) app, (f, n) app, m) t =
 fun pure nat p1 p2 ->
  let ( >>= ) x fn = p1.monad#bind fn x in
  let review f =
    p2.review f >>= fun d ->
    pure d >>= fun d -> p1.review d
  in
  let preview a =
    p1.preview a >>= function
    | Some c_addr -> nat c_addr >>= fun c -> p2.preview c
    | None -> p1.monad#return None
  in
  { monad = p1.monad; review; preview }

let ( >> ) :
    type m a b c d e f.
    (a, b, c, d, m) t -> (c, d, e, f, m) t -> (a, b, e, f, m) t =
 fun f g ->
  let ( >=> ) = Monad.kliesli f.monad in
  let review = g.review >=> f.review in
  let preview x =
    f.preview x
    |> f.monad#bind (function
         | Some a -> g.preview a
         | None -> f.monad#return None)
  in
  { monad = f.monad; review; preview }

type (_, 'm) t_list =
  | ( :: ) :
      ('s, 't, 'a, 'b, 'm) t * ('l, 'm) t_list
      -> (('s, 't, 'a, 'b, 'm) t * 'l, 'm) t_list
  | [] : (unit, 'm) t_list

let id monad =
  { monad; review = monad#return; preview = (fun s -> Some s |> monad#return) }
