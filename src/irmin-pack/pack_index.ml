(* Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module type S = sig
  include Index.S with type value = int64 * int * char

  val v :
    ?flush_callback:(unit -> unit) ->
    ?fresh:bool ->
    ?readonly:bool ->
    ?throttle:[ `Block_writes | `Overcommit_memory ] ->
    log_size:int ->
    string ->
    t

  val find : t -> key -> value option
  val add : ?overcommit:bool -> t -> key -> value -> unit
  val close : t -> unit
  val merge : t -> unit

  module Stats = Index.Stats
end

module Make (K : Irmin.Hash.S) = struct
  module Key = struct
    type t = K.t [@@deriving irmin]

    let hash = Irmin.Type.(unstage (short_hash K.t)) ?seed:None
    let hash_size = 30
    let equal = Irmin.Type.(unstage (equal K.t))
    let encode = Irmin.Type.(unstage (to_bin_string K.t))
    let encoded_size = K.hash_size
    let decode_bin = Irmin.Type.(unstage (decode_bin K.t))

    let decode s off =
      let _, v = decode_bin s off in
      v
  end

  module Val = struct
    type t = int64 * int * char [@@deriving irmin]

    let to_bin_string =
      Irmin.Type.(unstage (to_bin_string (triple int64 int32 char)))

    let encode (off, len, kind) = to_bin_string (off, Int32.of_int len, kind)
    let decode_bin = Irmin.Type.(unstage (decode_bin (triple int64 int32 char)))

    let decode s off =
      let off, len, kind = snd (decode_bin s off) in
      (off, Int32.to_int len, kind)

    let encoded_size = (64 / 8) + (32 / 8) + 1
  end

  module Stats = Index.Stats
  module Index = Index_unix.Make (Key) (Val) (Index.Cache.Unbounded)
  include Index

  (** Implicit caching of Index instances. TODO: Require the user to pass Pack
      instance caches explicitly. See
      https://github.com/mirage/irmin/issues/1017. *)
  let cache = Index.empty_cache ()

  let v = Index.v ~cache
  let add ?overcommit t k v = replace ?overcommit t k v
  let find t k = match find t k with exception Not_found -> None | h -> Some h
  let close t = Index.close t
end
