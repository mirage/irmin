(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

(** [Trace_common] contains utility to simplify the management of files using
    the following layout:

    {v
    - Magic (Magic.t, 8 bytes)
    - Version (int32, 4 bytes)
    - Length of header (varint, >=1 byte)
    - Header (header_t, _ bytes)
    - Arbitrary long series of rows, of unspecified length, each prefixed by their length:
      - Length of row (varint, >=1 byte)
      - Row (row_t, _ bytes)
    v}

    This file is meant to be used from Tezos. OCaml version 4.09 and the 32bit
    architecture should be supported.

    {3 Example}

    {[
      module Example = struct
        module V2 = struct
          let version = 2

          type header = unit [@@deriving repr]
          type row = [ `A | `B | `C ] [@@deriving repr]
        end

        module V1 = struct
          let version = 1

          type header = unit [@@deriving repr]
          type row = [ `A | `B ] [@@deriving repr]

          let to_v2 x = (x :> V2.row)
        end

        module V0 = struct
          let version = 0

          type header = unit [@@deriving repr]
          type row = [ `A of int | `B of int ] [@@deriving repr]

          let to_v1 = function `A _ -> `A | `B _ -> `B
        end

        module Latest = V2
        include Latest

        include Trace_common.Io (struct
          module Latest = Latest

          let magic = Trace_common.Magic.of_string "Magique_"

          let get_version_converter = function
            | 2 ->
                Trace_common.Version_converter
                  {
                    header_t = V2.header_t;
                    row_t = V2.row_t;
                    upgrade_header = Fun.id;
                    upgrade_row = Fun.id;
                  }
            | 1 ->
                Version_converter
                  {
                    header_t = V1.header_t;
                    row_t = V1.row_t;
                    upgrade_header = Fun.id;
                    upgrade_row = V1.to_v2;
                  }
            | 0 ->
                Version_converter
                  {
                    header_t = V0.header_t;
                    row_t = V0.row_t;
                    upgrade_header = Fun.id;
                    upgrade_row = (fun x -> V0.to_v1 x |> V1.to_v2);
                  }
            | i -> Fmt.invalid_arg "Unknown Example version %d" i
        end)
      end
    ]} *)

module Seq = struct
  include Seq

  (* Backported from ocaml 4.11 *)
  let rec unfold f u () =
    match f u with None -> Nil | Some (x, u') -> Cons (x, unfold f u')
end

module Magic : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end = struct
  type t = string

  let of_string s =
    if String.length s <> 8 then
      invalid_arg "Magic.of_string, string should have 8 chars";
    s

  let to_string s = s
  let pp ppf s = Format.fprintf ppf "%s" (String.escaped s)
end

type ('latest_header, 'latest_row, 'header, 'row) version_converter' = {
  header_t : 'header Repr.ty;
  row_t : 'row Repr.ty;
  upgrade_header : 'header -> 'latest_header;
  upgrade_row : 'row -> 'latest_row;
}
(** Contains everything needed to read a file as if it is written with the
    lastest version. *)

(** A box containing the above record *)
type ('latest_header, 'latest_row) version_converter =
  | Version_converter :
      ('latest_header, 'latest_row, 'header, 'row) version_converter'
      -> ('latest_header, 'latest_row) version_converter

module type File_format = sig
  (** The latest up-to-date definition of the file format *)
  module Latest : sig
    val version : int

    type header [@@deriving repr]
    type row [@@deriving repr]
  end

  val magic : Magic.t

  val get_version_converter :
    int -> (Latest.header, Latest.row) version_converter
end

(** Very similar to what can be found in "repr/type_binary.ml", but working
    straight off channels.

    [Var_int.read_exn] reads the chars one by one from the provided [chan]. The
    recursion stops as soon as a read char has its 8th bit equal to [0].

    [Var_int.write] could be implemented using [Repr.encode_bin int], but since
    [read_exn] isn't implemented using repr, [write] isn't either. *)
module Var_int = struct
  let chars =
    Array.init 256 (fun i -> Bytes.unsafe_to_string (Bytes.make 1 (Char.chr i)))

  let write : int -> out_channel -> unit =
    let int i k =
      let rec aux n k =
        if n >= 0 && n < 128 then k chars.(n)
        else
          let out = 128 lor (n land 127) in
          k chars.(out);
          aux (n lsr 7) k
      in
      aux i k
    in
    fun i chan -> int i (output_string chan)

  let read_exn : in_channel -> int =
   fun chan ->
    let max_bits = Sys.word_size - 1 in
    let rec aux n p =
      if p >= max_bits then failwith "Failed to decode varint";
      let i = input_char chan |> Char.code in
      let n = n + ((i land 127) lsl p) in
      if i >= 0 && i < 128 then n else aux n (p + 7)
    in
    aux 0 0
end

(** Derive the IO operations from a file format. Only the write operations are
    performance sensitive, the read operations are not. *)
module Io (Ff : File_format) = struct
  let decode_i32 = Repr.(decode_bin int32 |> unstage)
  let encode_i32 = Repr.(encode_bin int32 |> unstage)
  let encode_lheader = Repr.(encode_bin Ff.Latest.header_t |> unstage)
  let encode_lrow = Repr.(encode_bin Ff.Latest.row_t |> unstage)
  let magic = Ff.magic

  let read_with_prefix_exn : (string -> int ref -> 'a) -> in_channel -> 'a =
   fun decode chan ->
    (* First read the prefix *)
    let len = Var_int.read_exn chan in
    (* Then read the repr. *)
    let pos_ref = ref 0 in
    let v =
      (* This could fail if [len] is not long enough for repr (corruption) *)
      decode (really_input_string chan len) pos_ref
    in
    if len <> !pos_ref then
      Fmt.failwith
        "An value read in the Trace was expected to take %d bytes, but it took \
         only %d."
        len !pos_ref;
    v

  let decoded_seq_of_encoded_chan_with_prefixes :
      'a Repr.ty -> in_channel -> 'a Seq.t =
   fun repr chan ->
    let decode = Repr.decode_bin repr |> Repr.unstage in
    let produce_row () =
      try
        let row = read_with_prefix_exn decode chan in
        Some (row, ())
      with End_of_file -> None
    in
    Seq.unfold produce_row ()

  let open_reader :
      Eio.Fs.dir_ty Eio.Path.t -> Ff.Latest.header * Ff.Latest.row Seq.t =
   fun path ->
    let path = Eio.Path.native_exn path in
    let chan = open_in_bin path in
    let len = LargeFile.in_channel_length chan in
    if len < 12L then
      Fmt.invalid_arg "File '%s' should be at least 12 byte long." path;

    let magic = Magic.of_string (really_input_string chan 8) in
    if magic <> Ff.magic then
      Fmt.invalid_arg "File '%s' has magic '%a'. Expected '%a'." path Magic.pp
        magic Magic.pp Ff.magic;

    let (Version_converter vc) =
      let pos_ref = ref 0 in
      let version = decode_i32 (really_input_string chan 4) pos_ref in
      assert (!pos_ref = 4);
      Ff.get_version_converter (Int32.to_int version)
    in

    let header =
      let decode_header = Repr.(decode_bin vc.header_t |> unstage) in
      read_with_prefix_exn decode_header chan |> vc.upgrade_header
    in
    let seq =
      decoded_seq_of_encoded_chan_with_prefixes vc.row_t chan
      |> Seq.map vc.upgrade_row
    in
    (header, seq)

  type writer = { path : string; channel : out_channel; buffer : Buffer.t }

  let create_file path header =
    let path = Eio.Path.native_exn path in
    let channel = open_out path in
    let buffer = Buffer.create 0 in
    output_string channel (Magic.to_string Ff.magic);
    encode_i32 (Int32.of_int Ff.Latest.version) (output_string channel);
    encode_lheader header (Buffer.add_string buffer);
    Var_int.write (Buffer.length buffer) channel;
    output_string channel (Buffer.contents buffer);
    Buffer.clear buffer;
    { path; channel; buffer }

  let append_row { channel; buffer; _ } row =
    encode_lrow row (Buffer.add_string buffer);
    Var_int.write (Buffer.length buffer) channel;
    output_string channel (Buffer.contents buffer);
    Buffer.clear buffer

  let flush { channel; _ } = flush channel
  let close { channel; _ } = close_out channel

  let remove { channel; path; _ } =
    close_out channel;
    Sys.remove path
end
