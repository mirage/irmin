(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
include Watch_intf

let src = Logs.Src.create "irmin.watch" ~doc:"Irmin watch notifications"

module Log = (val Logs.src_log src : Logs.LOG)

let none _ _ =
  Printf.eprintf "Listen hook not set!\n%!";
  assert false

let listen_dir_hook = ref none
let watch_switch = Atomic.make None

type hook = int -> string -> (string -> unit) -> unit -> unit

let set_listen_dir_hook (h : hook) = listen_dir_hook := h

type eio_hook =
  int -> Eio.Fs.dir_ty Eio.Path.t -> (string -> unit) -> unit -> unit

let set_watch_switch sw = Atomic.set watch_switch (Some sw)

let get_watch_switch () =
  match Atomic.get watch_switch with
  | Some sw -> sw
  | None ->
      invalid_arg
        "Irmin.Backend.Watch: set_watch_switch must be called before adding a \
         watch"

(* Adding a watch checks that the switch is set, so that a missing switch fails
   at once rather than when the watch is notified. *)
let check_watch_switch () =
  let _ : Eio.Switch.t = get_watch_switch () in
  ()

let set_listen_dir_eio_hook ~sw fs (h : eio_hook) =
  set_watch_switch sw;
  set_listen_dir_hook (fun id dir f -> h id Eio.Path.(fs / dir) f)

let id () =
  let c = ref 0 in
  fun () ->
    incr c;
    !c

let global = id ()
let workers_r = ref 0
let workers () = !workers_r

(* [stream_iter f s] calls [f] on the elements of [s] until [None], or until
   the fiber is cancelled: [Eio.Stream.take] does not check it when [s] is not
   empty. *)
let rec stream_iter f s =
  Eio.Fiber.check ();
  match Eio.Stream.take s with
  | Some v ->
      f v;
      stream_iter f s
  | None -> ()

(* [drop_pending s n] is [n] plus the number of notifications removed from [s],
   up to its end. *)
let rec drop_pending s n =
  match Eio.Stream.take_nonblocking s with
  | Some (Some _) -> drop_pending s (n + 1)
  | Some None | None -> n

let scheduler () =
  (* [p] is the function pushing to the running worker, and its switch. *)
  let p = ref None in
  (* [running] is held by the worker running the callbacks. The order of the
     callbacks relies on [Eio.Mutex] handing the lock to its waiters in turn.
     It is used with [use_ro], as [use_rw] would poison it when a worker is
     cancelled. *)
  let running = Eio.Mutex.create () in
  (* FIXME: we would like to skip some updates if more recent ones are at the
     back of the queue. *)
  let start_worker sw =
    let stream = Eio.Stream.create max_int in
    let push = Eio.Stream.add stream in
    let worker = Some (push, sw) in
    (* [Eio.Fiber.fork] runs the worker at once, so it sets [p] before the next
       notification. The worker counts itself while it runs. It can also end
       because [sw] is cancelled: it then lets the next notification start a
       new one, which waits for [running], so that the callbacks run one at a
       time and in order. *)
    Eio.Fiber.fork ~sw (fun () ->
        p := worker;
        incr workers_r;
        Fun.protect
          ~finally:(fun () ->
            decr workers_r;
            if !p == worker then p := None;
            match drop_pending stream 0 with
            | 0 -> ()
            | n ->
                [%log.warn
                  "a cancelled watch worker dropped its pending notifications: \
                   %d"
                  n])
          (fun () ->
            Eio.Mutex.use_ro running (fun () ->
                stream_iter (fun f -> f ()) stream)));
    push
  in
  let push elt =
    match !p with
    | Some (push, sw) when Option.is_none (Eio.Switch.get_error sw) -> push elt
    (* No worker, or its switch is off. *)
    | Some _ | None -> (
        (* The errors are logged rather than raised: [push] runs under the lock
           of the watch state, which an exception would poison.
           [get_watch_switch] does not fail, as the switch was set before the
           watches were added, and is never unset. *)
        let sw = get_watch_switch () in
        (* [Eio.Fiber.fork] starts no fiber on a switch that is off. *)
        match Eio.Switch.get_error sw with
        | None -> (
            (* [Eio.Fiber.fork] raises [Invalid_argument] if [sw] belongs to
               another domain. *)
            match start_worker sw with
            | push -> push elt
            | exception Invalid_argument msg ->
                [%log.err "dropping a watch notification: %s" msg])
        | Some e ->
            [%log.warn
              "dropping a watch notification, as its switch is off: %a" Fmt.exn
                e])
  in
  let clean () =
    match !p with
    | None -> ()
    | Some (push, _) ->
        p := None;
        push None
  in
  let enqueue v = push (Some v) in
  (clean, enqueue)

module Make (K : sig
  type t

  val t : t Type.t
end) (V : sig
  type t

  val t : t Type.t
end) =
struct
  type key = K.t
  type value = V.t
  type watch = int

  module KMap = Map.Make (struct
    type t = K.t

    let compare = Type.(unstage (compare K.t))
  end)

  module IMap = Map.Make (struct
    type t = int

    let compare (x : int) (y : int) = compare x y
  end)

  type key_handler = value Diff.t -> unit
  type all_handler = key -> value Diff.t -> unit

  let pp_value = Type.pp V.t
  let equal_opt_values = Type.(unstage (equal (option V.t)))
  let equal_keys = Type.(unstage (equal K.t))

  type listening = {
    stop : unit -> unit;
    (* remove the listen hook. *)
    mutable users : int; (* number of [listen_dir] calls not stopped yet. *)
  }

  type t = {
    id : int;
    (* unique watch manager id. *)
    lock : Eio.Mutex.t;
    (* protect [keys] and [glob]. *)
    mutable next : int;
    (* next id, to identify watch handlers. *)
    mutable keys : (key * value option * key_handler) IMap.t;
    (* key handlers. *)
    mutable glob : (value KMap.t * all_handler) IMap.t;
    (* global handlers. *)
    enqueue : (unit -> unit) -> unit;
    (* enqueue notifications. *)
    clean : unit -> unit;
    (* destroy the notification thread. *)
    listen_lock : Eio.Mutex.t;
    (* taken before [lock]: serialise the calls to the listen hook, which may
       suspend, and protect [listening]. *)
    mutable listening : listening option;
    (* the listen hook, if any. *)
    mutable notifications : int; (* number of notifcations. *)
  }

  let stats t = (IMap.cardinal t.keys, IMap.cardinal t.glob)

  let to_string t =
    let k, a = stats t in
    let listeners = match t.listening with Some l -> l.users | None -> 0 in
    Printf.sprintf "[%d: %dk/%dg|%d]" t.id k a listeners

  let next t =
    let id = t.next in
    t.next <- id + 1;
    id

  let is_empty t = IMap.is_empty t.keys && IMap.is_empty t.glob

  let clear_unsafe t =
    t.keys <- IMap.empty;
    t.glob <- IMap.empty;
    t.next <- 0;
    t.clean ()

  (* [listening] is only changed before or after calling the hook, so it stays
     consistent if the hook raises or is cancelled: the mutex need not be
     poisoned. *)
  let with_listen_lock t f = Eio.Mutex.use_ro t.listen_lock f

  let stop_listening_unsafe t =
    match t.listening with
    | None -> ()
    | Some l ->
        t.listening <- None;
        l.stop ()

  (* [clear] and the stop functions clean up: they are not cancelled, even
     while they wait for a hook to start. *)
  let clear t =
    Eio.Cancel.protect @@ fun () ->
    with_listen_lock t @@ fun () ->
    Eio.Mutex.use_rw ~protect:true t.lock (fun () -> clear_unsafe t);
    stop_listening_unsafe t

  let v () =
    let lock = Eio.Mutex.create () in
    let clean, enqueue = scheduler () in
    {
      lock;
      clean;
      enqueue;
      id = global ();
      next = 0;
      keys = IMap.empty;
      glob = IMap.empty;
      listen_lock = Eio.Mutex.create ();
      listening = None;
      notifications = 0;
    }

  let unwatch_unsafe t id =
    [%log.debug "unwatch %s: id=%d" (to_string t) id];
    let glob = IMap.remove id t.glob in
    let keys = IMap.remove id t.keys in
    t.glob <- glob;
    t.keys <- keys

  let unwatch t id =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        unwatch_unsafe t id;
        if is_empty t then t.clean ())

  let mk old value =
    match (old, value) with
    | None, None -> assert false
    | Some v, None -> `Removed v
    | None, Some v -> `Added v
    | Some x, Some y -> `Updated (x, y)

  let protect f () =
    let log e bt =
      [%log.err
        "watch callback got: %a\n%s" Fmt.exn e
          (Printexc.raw_backtrace_to_string bt)]
    in
    try f () with
    | Eio.Cancel.Cancelled _ as e ->
        let bt = Printexc.get_raw_backtrace () in
        (* The cancellation of the worker ends it. The callback may also raise
           [Cancelled] for a context of its own, which is an error. *)
        Eio.Fiber.check ();
        log e bt
    | e -> log e (Printexc.get_raw_backtrace ())

  let pp_option = Fmt.option ~none:(Fmt.any "<none>")
  let pp_key = Type.pp K.t

  let notify_all_unsafe t key value =
    let todo = ref [] in
    let glob =
      IMap.fold
        (fun id ((init, f) as arg) acc ->
          let fire old_value =
            todo :=
              protect (fun () ->
                  [%log.debug
                    "notify-all[%d.%d:%a]: %d firing! (%a -> %a)" t.id id pp_key
                      key t.notifications (pp_option pp_value) old_value
                      (pp_option pp_value) value];
                  t.notifications <- t.notifications + 1;
                  f key (mk old_value value))
              :: !todo;
            let init =
              match value with
              | None -> KMap.remove key init
              | Some v -> KMap.add key v init
            in
            IMap.add id (init, f) acc
          in
          let old_value =
            try Some (KMap.find key init) with Not_found -> None
          in
          if equal_opt_values old_value value then (
            [%log.debug
              "notify-all[%d:%d:%a]: same value, skipping." t.id id pp_key key];
            IMap.add id arg acc)
          else fire old_value)
        t.glob IMap.empty
    in
    t.glob <- glob;
    match !todo with
    | [] -> ()
    | ts ->
        (* Check iter_p *)
        t.enqueue (fun () -> List.iter (fun x -> x ()) ts)

  let notify_key_unsafe t key value =
    let todo = ref [] in
    let keys =
      IMap.fold
        (fun id ((k, old_value, f) as arg) acc ->
          if not (equal_keys key k) then IMap.add id arg acc
          else if equal_opt_values value old_value then (
            [%log.debug
              "notify-key[%d.%d:%a]: same value, skipping." t.id id pp_key key];
            IMap.add id arg acc)
          else (
            todo :=
              protect (fun () ->
                  [%log.debug
                    "notify-key[%d:%d:%a] %d firing! (%a -> %a)" t.id id pp_key
                      key t.notifications (pp_option pp_value) old_value
                      (pp_option pp_value) value];
                  t.notifications <- t.notifications + 1;
                  f (mk old_value value))
              :: !todo;
            IMap.add id (k, value, f) acc))
        t.keys IMap.empty
    in
    t.keys <- keys;
    match !todo with
    | [] -> ()
    | ts ->
        (* Check iter_p *)
        t.enqueue (fun () -> List.iter (fun x -> x ()) ts)

  let notify t key value =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        if is_empty t then ()
        else (
          notify_all_unsafe t key value;
          notify_key_unsafe t key value))

  let watch_key_unsafe t key ?init f =
    let id = next t in
    [%log.debug "watch-key %s: id=%d" (to_string t) id];
    t.keys <- IMap.add id (key, init, f) t.keys;
    id

  let watch_key t key ?init f =
    check_watch_switch ();
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        let id = watch_key_unsafe t ?init key f in
        id)

  let kmap_of_alist l =
    List.fold_left (fun map (k, v) -> KMap.add k v map) KMap.empty l

  let watch_unsafe t ?(init = []) f =
    let id = next t in
    [%log.debug "watch %s: id=%d" (to_string t) id];
    t.glob <- IMap.add id (kmap_of_alist init, f) t.glob;
    id

  let watch t ?init f =
    check_watch_switch ();
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        let id = watch_unsafe t ?init f in
        id)

  let listen_dir t dir ~key ~value =
    check_watch_switch ();
    let on_change file =
      match key file with
      | None -> ()
      | Some key ->
          let rec read n =
            let value = value key in
            let n' = t.notifications in
            if n = n' then notify t key value
            else (
              [%log.debug "Stale event, trying reading again"];
              read n')
          in
          read t.notifications
    in
    let l =
      with_listen_lock t @@ fun () ->
      let l =
        match t.listening with
        | Some l ->
            [%log.debug "%s: already listening on %s" (to_string t) dir];
            l
        | None ->
            [%log.debug "%s: start listening to %s" (to_string t) dir];
            let l = { stop = !listen_dir_hook t.id dir on_change; users = 0 } in
            t.listening <- Some l;
            l
      in
      l.users <- l.users + 1;
      l
    in
    (* Each stop function counts once, and does nothing once [clear] removed
       the hook it was counted in. *)
    let stopped = ref false in
    fun () ->
      Eio.Cancel.protect @@ fun () ->
      with_listen_lock t @@ fun () ->
      if not !stopped then (
        stopped := true;
        l.users <- l.users - 1;
        match t.listening with
        | Some l' when l' == l && l.users = 0 ->
            [%log.debug "%s: stop listening to %s" (to_string t) dir];
            stop_listening_unsafe t
        | Some _ | None -> ())
end
