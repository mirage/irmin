(** IO effects *)

type ('a, 'b) t = ('a, 'b) IO_intf.t

module type S = sig
  include IO_intf.S
  module Syntax : IO_intf.Syntax with type ('a, _) t := 'a t
end

module type H = sig
  include IO_intf.H
  module Syntax : IO_intf.Syntax with type ('a, 'io) t := ('a, 'io) t
end

module type Syntax = IO_intf.Syntax
module type List = IO_intf.List

module Syntax (IO : IO_intf.S0) = struct
  let ( let* ) = IO.bind
  let ( and* ) = IO.both
  let ( let+ ) x f = IO.map f x
  let ( and+ ) = IO.both
end

module Syntax' (IO : IO_intf.H) = struct
  let ( let* ) = IO.bind
  let ( and* ) = IO.both
  let ( let+ ) x f = IO.map f x
  let ( and+ ) = IO.both
end

module Abstract = struct
  module M = struct
    type (_, _) t_ =
      | Return : 'a -> ('a, _) t_
      | Bind : ('a, 'io) t_ * ('a -> ('b, 'io) t_) -> ('b, 'io) t_
      | Map : ('a -> 'b) * ('a, 'io) t_ -> ('b, 'io) t_
      | Both : ('a, 'io) t_ * ('b, 'io) t_ -> ('a * 'b, 'io) t_
      | Fail : exn -> _ t_
      | Catch : (unit -> ('a, 'io) t_) * (exn -> ('a, 'io) t_) -> ('a, 'io) t_
      | Promise : ('a, 'io) t -> ('a, 'io) t_
      | Async : (unit -> (unit, 'io) t_) -> (unit, 'io) t_

    type (+_, _) t

    (* The type system cannot see that t is covariant in its parameter.
       Use the Force to convince it.
       From: https://github.com/let-def/lwd/blob/master/lib/lwd/lwd.ml

       See http://gallium.inria.fr/~scherer/research/variance_gadts/short_talk.pdf
       for a proof.
    *)
    external inj : ('a, 'io) t_ -> ('a, 'io) t = "%identity"
    external prj : ('a, 'io) t -> ('a, 'io) t_ = "%identity"

    let fproj f x = prj (f x)
    let return x = inj (Return x)
    let bind x f = inj (Bind (prj x, fproj f))
    let map f x = inj (Map (f, prj x))
    let both x y = inj (Both (prj x, prj y))
    let fail e = inj (Fail e)
    let catch f h = inj (Catch (fproj f, fproj h))
    let promise v = inj (Promise v)
    let async f = inj (Async (fproj f))
  end

  include M
  module Syntax = Syntax' (M)
end

module type Higher = sig
  include IO_intf.Higher

  val run : ('a, Higher.s) Abstract.t -> 'a t
  val abstract : 'a t -> ('a, Higher.s) Abstract.t
end

module Higher (S : IO_intf.S) = struct
  module Higher = struct
    module M = struct
      type s
      type nonrec +'a t = ('a, s) t

      external inj : 'a S.t -> 'a t = "%identity"
      external prj : 'a t -> 'a S.t = "%identity"

      let fproj f x = prj (f x)
      let return x = inj (S.return x)
      let bind x f = inj (S.bind (prj x) (fproj f))
      let map f x = inj (S.map f (prj x))
      let both x y = inj (S.both (prj x) (prj y))
      let fail e = inj (S.fail e)
      let catch f h = inj (S.catch (fproj f) (fproj h))
      let async t = inj (S.async (fproj t))

      module Mutex = struct
        include S.Mutex

        let with_lock t f = inj (S.Mutex.with_lock t (fun () -> prj (f ())))
      end

      module Stream = struct
        include S.Stream

        let iter_s f t = inj (iter_s (fun x -> prj (f x)) t)
      end
    end

    include M
    module Syntax = Syntax (M)
  end

  include S

  external inj : 'a t -> 'a Higher.t = "%identity"
  external prj : 'a Higher.t -> 'a t = "%identity"

  module Syntax = Syntax (S)

  let rec run : type a. (a, 'io) Abstract.t -> a t =
   fun t -> run_ (Abstract.prj t)

  and run_ : type a. (a, 'io) Abstract.t_ -> a t = function
    | Return x -> S.return x
    | Bind (x, f) -> S.bind (run_ x) (frun_ f)
    | Map (f, x) -> S.map f (run_ x)
    | Both (x, y) -> S.both (run_ x) (run_ y)
    | Fail e -> S.fail e
    | Catch (f, h) -> S.catch (frun_ f) (frun_ h)
    | Promise t -> prj t
    | Async f -> S.async (frun_ f)

  and frun_ : type a b. (a -> (b, 'io) Abstract.t_) -> a -> b t =
   fun f x -> run_ (f x)

  let abstract : type a. a S.t -> (a, Higher.s) Abstract.t =
   fun t -> Abstract.promise (Higher.inj t)
end

module Lwt : S with type 'a t = 'a Lwt.t = struct
  include Lwt

  let async f = return (async f)

  module Mutex = Lwt_mutex
  module Stream = Lwt_stream
end

module Direct : S with type 'a t = 'a = struct
  module M = struct
    type +'a t = 'a

    external return : 'a -> 'a t = "%identity"

    let bind x f = f x
    let map f x = f x [@@inline always]
    let both x y = (x, y) [@@inline always]
    let fail = raise
    let catch f h = try f () with e -> h e
    let async f = ignore (Thread.create f ())
  end

  include M
  module Syntax = Syntax (M)

  module Mutex = struct
    module S = Semaphore_compat.Semaphore.Binary

    type t = S.t

    let create () = S.make false

    let is_empty t =
      let acquired = S.try_acquire t in
      if acquired then S.release t;
      not acquired

    let with_lock t f =
      S.acquire t;
      Fun.protect ~finally:(fun () -> S.release t) f
  end

  module Stream = struct
    type 'a t = { channel : 'a option Event.channel; mutable closed : bool }

    exception Closed

    let create () =
      let c = { channel = Event.new_channel (); closed = false } in
      let push e =
        if c.closed then fail Closed
        else
          match e with
          | None ->
              (* signal readers that the stream is closed. *)
              ignore (Event.send c.channel None);
              c.closed <- true
          | Some _ -> ignore (Event.send c.channel e)
      in
      (c, push)

    let rec iter_s f t =
      if t.closed then ()
      else
        match Event.sync (Event.receive t.channel) with
        | None -> ()
        | Some e ->
            f e;
            iter_s f t
  end
end

module Gen (S : S) = struct
  type (+'a, _) t = 'a S.t

  let return = S.return
  let bind = S.bind
  let map = S.map
  let both = S.both
  let fail = S.fail
  let catch = S.catch
  let async = S.async

  module Syntax = Syntax (S)
end

module List' (S : H) = struct
  open S.Syntax

  let apply f x = try f x with exn -> S.fail exn

  let fold_left_s f acc l =
    List.fold_left
      (fun acc x ->
        let* acc = acc in
        apply (f acc) x)
      (S.return acc) l

  let iter_p f l =
    List.fold_left
      (fun acc x ->
        let+ () = acc and+ () = apply f x in
        ())
      (S.return ()) l

  let iter_s f l =
    List.fold_left
      (fun acc x ->
        let* () = acc in
        let+ () = apply f x in
        ())
      (S.return ()) l

  let map_p f l =
    List.fold_left
      (fun acc x ->
        let+ acc = acc and+ x = apply f x in
        x :: acc)
      (S.return []) l

  let map_s f l =
    List.fold_left
      (fun acc x ->
        let* acc = acc in
        let+ x = apply f x in
        x :: acc)
      (S.return []) l

  let filter_map_s f l =
    let rec aux acc = function
      | [] -> S.return (List.rev acc)
      | hd :: tl -> (
          let* x = apply f hd in
          match x with Some v -> aux (v :: acc) tl | None -> aux acc tl)
    in
    aux [] l

  let filter_map_p f l =
    let rec aux acc = function
      | [] -> S.return acc
      | t :: ts -> (
          let* x = t in
          match x with Some v -> aux (v :: acc) ts | None -> aux acc ts)
    in
    let ts = List.rev_map (apply f) l in
    aux [] ts

  let rec for_all_s f l =
    match l with
    | [] -> S.return true
    | x :: l ->
        let* r = apply f x in
        if r then for_all_s f l else S.return false

  let for_all_p f l =
    let+ bl = map_p f l in
    List.for_all (fun x -> x) bl
end

module List (S : S) = List' (Gen (S))
