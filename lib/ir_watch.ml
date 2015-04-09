(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log = Log.Make(struct let section = "WATCH" end)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type 'a diff = [`Updated of 'a * 'a | `Removed of 'a | `Added of 'a]

module type S = sig
  type key
  type value
  type watch
  type t
  val stats: t -> int
  val notify: t -> key -> value option -> unit Lwt.t
  val create: unit -> t
  val clear: t -> unit
  val watch: t -> (key -> value diff -> unit Lwt.t) -> watch Lwt.t
  val unwatch: t -> watch -> unit Lwt.t
  val listen_dir: t -> string
    -> key:(string -> key option)
    -> value:(key -> value option Lwt.t)
    -> unit
end

let listen_dir_hook =
  ref (fun _dir _fn ->
      Log.error "Listen hook not set!";
      assert false
    )

let set_listen_dir_hook fn =
  listen_dir_hook := fn

let id () =
  let c = ref 0 in
  fun () -> incr c; !c

let global = id ()

module Make (K: Tc.S0) (V: Tc.S0) = struct

  type key = K.t
  type value = V.t
  type watch = int
  module OV = Tc.Option(V)

  type handler = key -> value diff -> unit Lwt.t

  type t = {
    id: int;
    lock: Lwt_mutex.t;
    mutable next: int;
    mutable ws: (int * value option * handler) list;
    enqueue: ((unit -> unit Lwt.t) -> unit Lwt.t) Lazy.t;
  }

  let stats t = List.length t.ws
  let to_string t = Printf.sprintf "%d: %d" t.id (stats t)
  let clear t = t.ws <- []; t.next <- 0

  let enqueue () = lazy (
    let stream, push = Lwt_stream.create () in
    Lwt.async (fun () -> Lwt_stream.iter_s (fun f -> f ()) stream);
    function v ->
      let t, u = Lwt.task () in
      let todo () = v () >|= Lwt.wakeup u in
      push (Some todo);
      t
  )

  let create () = {
    lock = Lwt_mutex.create (); enqueue = enqueue ();
    id = global (); next = 0; ws = []
  }

  let unwatch_unsafe t id =
    let ws = List.filter (fun (x, _, _) -> x <> id) t.ws in
    t.ws <- ws

  let unwatch t id =
    Lwt_mutex.with_lock t.lock (fun () ->
        unwatch_unsafe t id;
        Lwt.return_unit
      )

  let mk old value = match old, value with
    | None  , None   -> assert false
    | Some v, None   -> `Removed v
    | None  , Some v -> `Added v
    | Some x, Some y -> `Updated (x, y)

  let enqueue t = Lazy.force t.enqueue

  let notify t key value =
    let todo = ref [] in
    let ws = List.fold_left (fun acc (id, old_value, f) ->
        if OV.equal old_value value then (
          Log.debug "notify: same value!";
          (id, old_value, f) :: acc
        ) else (
          Log.debug "notify: firing %d.%d!" t.id id;
          todo := (fun () -> f key (mk old_value value)) :: !todo;
          (id, value, f) :: acc
        )
      ) [] t.ws
    in
    t.ws <- ws;
    enqueue t (fun () -> Lwt_list.iter_p (fun x -> x ()) !todo)

  let watch_unsafe t f =
    Log.debug "watch %s" (to_string t);
    let id = t.next in
    t.next <- id + 1;
    t.ws <- (id, None, f) :: t.ws;
    id

  let watch t f =
    Lwt_mutex.with_lock t.lock (fun () ->
        let id = watch_unsafe t f in
        Lwt.return id
      )

  let listen_dir (t:t) dir ~key ~value =
    Log.debug "Add a listen hook for %s" (to_string t);
    !listen_dir_hook t.id dir (fun file ->
        Log.debug "listen_dir_hook: %s %s" (to_string t) file;
        match key file with
        | None     -> Lwt.return_unit
        | Some key -> value key >>= notify t key
      )

end
