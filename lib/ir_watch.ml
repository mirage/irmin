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
  val listen_dir: t -> string
    -> key:(string -> key option)
    -> value:(key -> value option Lwt.t)
    -> unit
end

let listen_dir_hook =
  ref (fun _dir _fn -> ())

let set_listen_dir_hook fn =
  listen_dir_hook := fn

module Make (K: Tc.S0) (V: Tc.S0) = struct

  type key = K.t
  type value = V.t
  module OV = Tc.Option(V)

  type t = (K.t, (int * value option * (value option option -> unit)) list) Hashtbl.t

  let create () =
    Hashtbl.create 42

  let clear t =
    Hashtbl.clear t

  let unwatch t key id =
    let ws =
      try Hashtbl.find t key
      with Not_found -> []
    in
    let ws = List.filter (fun (x,_,_) -> x <> id) ws in
    (* close the clients *)
    List.iter (fun (_, _, f) -> f None) ws;
    match ws with
    | [] -> Hashtbl.remove t key
    | ws -> Hashtbl.replace t key ws

  let notify t key value =
    Log.debug "notify %a" force (show (module K) key);
    try
      let ws = Hashtbl.find t key in
      let ws = List.map (fun (id, old_value, f as w) ->
          if not (OV.equal old_value value) then (
            Log.debug "firing watch %a:%d" force (show (module K) key) id;
            try f (Some value); (id, value, f)
            with e ->
              unwatch t key id;
              raise e
          ) else w
        ) ws
      in
      Hashtbl.replace t key ws
    with Not_found ->
      ()

  let id =
    let c = ref 0 in
    fun () -> incr c; !c

  let watch (t:t) key value =
    Log.debug "watch %a" force (show (module K) key);
    let stream, push = Lwt_stream.create () in
    let id = id () in
    Ir_misc.hashtbl_add_multi t key (id, value, push);
    stream

  let listen_dir t dir ~key ~value =
    !listen_dir_hook dir (fun file ->
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
