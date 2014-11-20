(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt

module Log = Log.Make(struct let section = "RO" end)

module type STORE = sig
  type t
  type key
  type value
  type origin
  val create: unit -> t
  val read: t -> origin -> key -> value option Lwt.t
  val read_exn: t -> origin -> key -> value Lwt.t
  val mem: t -> origin -> key -> bool Lwt.t
  val list: t -> origin -> key list -> key list Lwt.t
  val dump: t -> origin -> (key * value) list Lwt.t
end

module type MAKER =
  functor (K: Tc.I0) ->
  functor (V: Tc.I0) ->
  functor (O: Tc.I0) ->
    STORE with type key = K.t and type value = V.t and type origin = O.t

module type BINARY = STORE with
  type key = Cstruct.t and type value = Cstruct.t and type origin = Cstruct.t

module type JSON = STORE with
  type key = Ezjsonm.t and type value = Ezjsonm.t and type origin = Ezjsonm.t

module Binary  (S: BINARY) (K: Tc.I0) (V: Tc.I0) (O: Tc.I0) = struct

  type t = S.t

  type origin = O.t

  type key = K.t

  type value = V.t

  let k_to_raw = Tc.write_cstruct (module K)
  let k_of_raw = Tc.read_cstruct (module K)
  let o_to_raw = Tc.write_cstruct (module O)

  let create () =
    S.create ()

  let read t origin key =
    S.read t (o_to_raw origin) (k_to_raw key) >>= function
    | None    -> return_none
    | Some ba -> return (Some (Tc.read_cstruct (module V) ba))

  let read_exn t origin key =
    read t origin key >>= function
    | Some v -> return v
    | None   -> fail (Ir_uid.Unknown (Tc.show (module K) key))

  let mem t origin key =
    S.mem t (o_to_raw origin) (k_to_raw key)

  let list t origin keys =
    let keys = List.map k_to_raw keys in
    S.list t (o_to_raw origin) keys >>= fun ks ->
    let ks = List.map k_of_raw ks in
    return ks

  let dump t origin =
    S.dump t (o_to_raw origin) >>= fun l ->
    Lwt_list.fold_left_s (fun acc (s, ba) ->
        let v = Tc.read_cstruct (module V) ba in
        return ((k_of_raw s, v) :: acc)
      ) [] l

end

module Json  (S: JSON) (K: Tc.I0) (V: Tc.I0) (O: Tc.I0) = struct

  type t = S.t

  type origin = O.t

  type key = K.t

  type value = V.t

  let k_to_json = Tc.to_json (module K)
  let k_of_json = Tc.of_json (module K)
  let o_to_json = Tc.to_json (module O)

  let create () =
    S.create ()

  let read t origin key =
    S.read t (o_to_json origin) (k_to_json key) >>= function
    | None   -> return_none
    | Some j -> return (Some (Tc.of_json (module V) j))

  let read_exn t origin key =
    read t origin key >>= function
    | Some v -> return v
    | None   -> fail (Ir_uid.Unknown (Tc.show (module K) key))

  let mem t origin key =
    S.mem t (o_to_json origin) (k_to_json key)

  let list t origin keys =
    let keys = List.map k_to_json keys in
    S.list t (o_to_json origin) keys >>= fun ks ->
    let ks = List.map k_of_json ks in
    return ks

  let dump t origin =
    S.dump t (o_to_json origin) >>= fun l ->
    Lwt_list.fold_left_s (fun acc (s, ba) ->
        let v = Tc.of_json (module V) ba in
        return ((k_of_json s, v) :: acc)
      ) [] l

end
