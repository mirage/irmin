(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Ir_misc.OP
open Lwt

module type S = sig
  type key
  type value
  type t
  val notify: t -> key -> value option -> unit
  val create: unit -> t
  val clear: t -> unit
  val watch: t -> key -> value option -> value option Lwt_stream.t
  val watch_all: t -> (key * value option) Lwt_stream.t
  val listen_dir: t -> string
    -> key:(string -> key option)
    -> value:(key -> value option Lwt.t)
    -> unit
end

let listen_dir_hook =
  ref (fun _id _dir _fn ->
      Log.error "Listen hook not set!";
      assert false
    )

let set_listen_dir_hook fn =
  listen_dir_hook := fn

module Make (K: Tc.S0) (V: Tc.S0) = struct

  type key = K.t
  type value = V.t
  module OV = Tc.Option(V)

  type key_notifier = int * value option * (value option option -> unit)
  type all_notifier = int * ((key * value option) option -> unit)

  type t = {
    keys: (K.t,  key_notifier list) Hashtbl.t;
    mutable all: all_notifier list;
  }

  let to_string t =
    Printf.sprintf "(%s | %d)"
      (String.concat " "
         (let l = Hashtbl.fold (fun k _ l -> k :: l) t.keys [] in
          List.map (Tc.show (module K)) l))
      (List.length t.all)

  let create () =
    { keys = Hashtbl.create 42; all = [] }

  let clear t =
    Hashtbl.clear t.keys;
    t.all <- []

  let close_key_notofier = fun (_, _, f) -> f None
  let close_all_notifier = fun (_, f) -> f None

  let unwatch_keys (t:t) key id =
    let ws =
      try Hashtbl.find t.keys key
      with Not_found -> []
    in
    let ws = List.filter (fun (x,_,_) -> x <> id) ws in
    (* close the clients *)
    List.iter close_key_notofier ws;
    match ws with
    | [] -> Hashtbl.remove t.keys key
    | ws -> Hashtbl.replace t.keys key ws

  let notify_keys (t:t) key value =
    try
      let ws = Hashtbl.find t.keys key in
      let ws =
        List.fold_left (fun acc (id, old_value, f as w) ->
            if not (OV.equal old_value value) then (
              Log.debug "firing key-watch %a:%d" force (show (module K) key) id;
              try f (Some value); (id, value, f) :: acc
              with e ->
                Log.error "notify-key: %s" (Printexc.to_string e);
                unwatch_keys t key id;
                acc
            ) else w :: acc
          ) [] ws
        |> List.rev
      in
      Hashtbl.replace t.keys key ws
    with Not_found ->
      ()

  let unwatch_all (t:t) id =
    let to_close, all = List.partition (fun (x, _) -> x = id) t.all in
    List.iter close_all_notifier to_close;
    t.all <- all

  let notify_all (t:t) key value =
    let all =
      List.fold_left (fun acc (id, f) ->
          Log.debug "firing all-watch %a:%d" force (show (module K) key) id;
          try f (Some (key, value)); (id, f) :: acc
          with e ->
            Log.debug "notify-all: %s" (Printexc.to_string e);
            unwatch_all t id;
            acc
        ) [] t.all
      |> List.rev
    in
    t.all <- all

  let notify t key value =
    Log.debug "notify %s %a" (to_string t) force (show (module K) key);
    notify_keys t key value;
    notify_all t key value

  let id =
    let c = ref 0 in
    fun () -> incr c; !c

  let watch (t:t) key value =
    Log.debug "watch %a" force (show (module K) key);
    let stream, push = Lwt_stream.create () in
    let id = id () in
    Ir_misc.hashtbl_add_multi t.keys key (id, value, push);
    stream

  let watch_all (t:t) =
    Log.debug "watch all";
    let stream, push = Lwt_stream.create () in
    let id = id () in
    t.all <- (id, push) :: t.all;
    stream

  let listen_dir (t:t) dir ~key ~value =
    let id = id () in
    !listen_dir_hook id dir (fun file ->
        Log.debug "listen_dir_hook: %s" file;
        match key file with
        | None     -> return_unit
        | Some key ->
          value key >>= fun value ->
          notify t key value;
          return_unit
      )

end

let lwt_stream_lift s =
  let (stream: 'a Lwt_stream.t option ref) = ref None in
  let rec get () =
    match !stream with
    | Some s -> Lwt_stream.get s
    | None   ->
      s >>= fun s ->
      stream := Some s;
      get ()
  in
  Lwt_stream.from get
