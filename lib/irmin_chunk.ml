(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

open Lwt.Infix
open Irmin

module Log = Log.Make(struct let section = "CHUNK" end)

module Conf = struct

  let minimum_size = 4000

  let chunk_size =
    Irmin.Private.Conf.key ~doc:"Size of chunk" "size"
      Irmin.Private.Conf.int 4666

end

let chunk_size = Conf.chunk_size

let err_invalid_size size =
  Printf.ksprintf invalid_arg "%d is an invalid chunk size" size

let config ?(config=Irmin.Private.Conf.empty) ?size () =
  let module C = Irmin.Private.Conf in
  let size = match size with
    | None   -> C.default Conf.chunk_size
    | Some v -> if v <= Conf.minimum_size then err_invalid_size v else v
  in
  C.add config Conf.chunk_size size

module AO (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V: RAW) = struct

  module AO = S(K)(V)
  type key = AO.key
  type value = AO.value

  type t = {
    db:AO.t;
    size:int;
    hash_length: int;
    nb_indirect: int;
    data_length:int;
  }

  module Chunk = struct

    type t =
      | Data
      | Indirect

    let hash_length = K.digest_size

    let l_type = 1
    let get_type x = Cstruct.get_uint8 x 0
    let set_type x v = Cstruct.set_uint8 x 0 v

    let l_len = 2
    let get_length x = Cstruct.LE.get_uint16 x l_type
    let set_length x v = Cstruct.LE.set_uint16 x l_type v

    let head_length = l_type + l_len

    let offset_payload = head_length

    let get_sub_key x pos =
      let offset = offset_payload + (pos * hash_length) in
      Cstruct.sub x offset hash_length

    let set_data src srcoff dst dstoff len =
      Cstruct.blit src srcoff dst (offset_payload + dstoff) len

    let extract_data src srcoff dst dstoff len =
      Cstruct.blit src (offset_payload + srcoff) dst dstoff len

    let type_of_int = function
      | Indirect -> 0
      | Data -> 1

    let int_of_type = function
      | 0 -> Indirect
      | 1 -> Data
      | _ -> failwith "Unknow type"

    let type_of_chunk x = (int_of_type (get_type x))

    let create t len typ =
      let c = Cstruct.create t.size in
      Cstruct.memset c 0x00;
      set_type c (type_of_int typ);
      set_length c len;
      c

  end

  module Tree = struct

    let get_child t node pos =
      let key = K.of_raw (Chunk.get_sub_key node pos) in
      AO.read_exn t.db key

    let walk_to_list t root =
      let rec aux_walk t node =
        match Chunk.type_of_chunk node with
        | Chunk.Data ->
          let dlen = Chunk.get_length node in
          let leaf = Cstruct.create dlen in
          Chunk.extract_data node 0 leaf 0 dlen;
          Lwt.return [leaf]
        | Chunk.Indirect ->
          let nbr_child = Chunk.get_length node in
          let rec loop i accu =
            if i < nbr_child then
              get_child t node i >>= fun x ->
              aux_walk t x       >>= fun y ->
              loop (i+1) (List.append accu y)
            else
              Lwt.return accu
          in
          loop 0 []
      in
      aux_walk t root

    let rec aux_botom_up t l r =
      match l with
      | [] -> Lwt.return r
      | l ->
        let nbr =
          if List.length l >= t.nb_indirect
          then t.nb_indirect
          else List.length l
        in
        let indir = Chunk.create t nbr Chunk.Indirect in
        let rec loop i l =
          if (i >= nbr) then l
          else
            let x = List.hd l in
            let rest = List.tl l in
            let offset_hash = i * t.hash_length in
            Chunk.set_data (K.to_raw x) 0 indir offset_hash t.hash_length;
            loop (i+1) rest
        in
        let calc = loop 0 l in
        AO.add t.db indir >>= fun x ->
        aux_botom_up t calc (x::r)

    let rec bottom_up t l =
      match l with
      | [] -> failwith "Error 1"
      | [e] -> Lwt.return e
      | l   -> aux_botom_up t l [] >>= bottom_up t

  end

  let create config task =
    let module C = Irmin.Private.Conf in
    let size = C.get config Conf.chunk_size in
    let data_length = size - Chunk.head_length - 1 in
    let hash_length = K.digest_size in
    let nb_indirect = data_length / hash_length in
    AO.create config task >>= fun t ->
    Lwt.return (fun a ->
        {db = t a; size; hash_length; nb_indirect; data_length}
      )

  let task t = AO.task t.db
  let config t = AO.config t.db

  let read t key =
    AO.read_exn t.db key >>= fun x ->
    match Chunk.type_of_chunk x with
    | Chunk.Data ->
      let dlen = Chunk.get_length x in
      let result = Cstruct.create dlen in
      Chunk.extract_data x 0 result 0 dlen;
      Lwt.return (Some result)
    | Chunk.Indirect ->
      Tree.walk_to_list t x >>= fun y ->
      let result = Cstruct.concat y in
      Lwt.return (Some result)

  let read_exn t key =
    AO.read_exn t.db key >>= fun x ->
    match Chunk.type_of_chunk x with
    | Chunk.Data ->
      let dlen = Chunk.get_length x in
      let result = Cstruct.create dlen in
      Chunk.extract_data x 0 result 0 dlen;
      Lwt.return result
    | Chunk.Indirect ->
      Tree.walk_to_list t x >>= fun y ->
      let result = Cstruct.concat y in
      Lwt.return result

  (* FIXME: this function is way too long *)
  let add t v =
    let value_length = Cstruct.len v in
    let rest_value_length = value_length mod t.data_length in
    if value_length <= t.data_length then (
      let chunk = Chunk.create t value_length Chunk.Data in
      Chunk.set_data v 0 chunk 0 value_length;
      AO.add t.db chunk
    ) else (
      let nbr_data_bloc, last_data_bloc_length =
        let x =  value_length / t.data_length in
        if rest_value_length = 0 then
          x, t.data_length
        else
          (x + 1), rest_value_length
      in
      let nbr_indir_bloc, last_indir_bloc_length =
        let x = nbr_data_bloc / t.nb_indirect in
        let y = nbr_data_bloc mod t.nb_indirect in
        if y = 0 then
          x, t.nb_indirect
        else
          (x + 1), y
      in
      let split_data_first_level offset_second_level =
        let number_chunk =
          if offset_second_level = nbr_indir_bloc - 1 then
            last_indir_bloc_length
          else
            t.nb_indirect
        in
        let indir = Chunk.create t number_chunk Chunk.Indirect in
        let rec loop offset_first_level =
          let dlen, max_loop =
            if offset_second_level = nbr_indir_bloc - 1 then
              if offset_first_level = last_indir_bloc_length - 1 then
                last_data_bloc_length, last_indir_bloc_length
              else
                t.data_length, last_indir_bloc_length
            else
              t.data_length, t.nb_indirect
          in
          if offset_first_level >= max_loop then
            Lwt.return_unit
          else
            let chunk = Chunk.create t dlen Chunk.Data in
            let offset_hash = offset_first_level * t.hash_length in
            let offset_value =
              (offset_second_level * t.nb_indirect * t.data_length)
              + (offset_first_level * t.data_length)
            in
            Chunk.set_data v offset_value chunk 0 dlen;
            let add_chunk () =
              AO.add t.db chunk >>= fun x ->
              Chunk.set_data (K.to_raw x) 0 indir offset_hash Chunk.hash_length;
              Lwt.return_unit
            in
            Lwt.join [add_chunk (); loop (offset_first_level + 1)]
        in
        loop 0 >>= fun _ ->
        AO.add t.db indir >>= fun x ->
        Lwt.return x
      in
      let main_loop () =
        let rec first_level i =
          match i with
          | e when e = nbr_indir_bloc ->
            Lwt.return []
          | e when e < nbr_indir_bloc ->
            split_data_first_level i >>= fun x ->
            first_level (i+1) >>= fun y ->
            Lwt.return (x::y)
          | _ -> failwith "Error 2"
        in
        first_level 0
      in
      main_loop () >>= Tree.bottom_up t
    )

  let mem t key = AO.mem t.db key

  (* this doesn't seem needed *)
  let iter _t (_fn : key -> value Lwt.t -> unit Lwt.t) =
    failwith "Irmin_chunk.iter: TODO"

end
