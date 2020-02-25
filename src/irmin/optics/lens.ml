open Irmin_root
open Higher

type ('s, 't, 'a, 'b, 'm) t = {
  monad : 'm monad;
  view : 's -> ('a, 'm) app;
  modify : ('a -> ('b, 'm) app) -> 's -> ('t, 'm) app;
}

type ('s, 't, 'a, 'b, 'm) ty = ('s, 't, 'a, 'b, 'm) t

type ('s, 'a, 'm) mono = ('s, 's, 'a, 'a, 'm) t

let v monad view modify = { monad; view; modify }

let modify { modify; _ } = modify

let update { modify; _ } b = modify (fun _ -> b)

let view { view; _ } = view

let ( >> ) l1 l2 =
  let ( >=> ) = Monad.kliesli l1.monad in
  {
    monad = l1.monad;
    modify = l2.modify >>> l1.modify;
    view = l1.view >=> l2.view;
  }

type (_, 'm) t_list =
  | ( :: ) :
      ('s, 't, 'a, 'b, 'm) ty * ('l, 'm) t_list
      -> (('s, 't, 'a, 'b, 'm) ty * 'l, 'm) t_list
  | [] : (unit, 'm) t_list
