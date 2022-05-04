(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

open Irmin.Export_for_backends
module Int63 = Optint.Int63
module Dict = Irmin_pack_unix.Dict

let get = function Some x -> x | None -> Alcotest.fail "None"
let sha1 x = Irmin.Hash.SHA1.hash (fun f -> f x)
let sha1_contents x = sha1 ("B" ^ x)

let rm_dir root =
  if Sys.file_exists root then (
    let cmd = Printf.sprintf "rm -rf %s" root in
    [%logs.info "exec: %s\n%!" cmd];
    let _ = Sys.command cmd in
    ())

let index_log_size = Some 1_000
let () = Random.self_init ()
let random_char () = char_of_int (Random.int 256)
let random_string n = String.init n (fun _i -> random_char ())
let random_letter () = char_of_int (Char.code 'a' + Random.int 26)
let random_letters n = String.init n (fun _i -> random_letter ())

module Conf = Irmin_tezos.Conf

module Schema = struct
  open Irmin
  module Metadata = Metadata.None
  module Contents = Contents.String_v2
  module Path = Path.String_list
  module Branch = Branch.String
  module Hash = Hash.SHA1
  module Node = Node.Generic_key.Make_v2 (Hash) (Path) (Metadata)
  module Commit = Commit.Generic_key.Make_v2 (Hash)
  module Info = Info.Default
end

module Contents = struct
  include Irmin.Contents.String

  let kind _ = Irmin_pack.Pack_value.Kind.Contents

  module H = Irmin.Hash.Typed (Irmin.Hash.SHA1) (Irmin.Contents.String)

  let hash = H.hash
  let encode_pair = Irmin.Type.(unstage (encode_bin (pair H.t t)))
  let decode_pair = Irmin.Type.(unstage (decode_bin (pair H.t t)))
  let length_header = Fun.const (Some `Varint)
  let encode_bin ~dict:_ ~offset_of_key:_ k x = encode_pair (k, x)

  let decode_bin ~dict:_ ~key_of_offset:_ ~key_of_hash:_ x pos_ref =
    let _, v = decode_pair x pos_ref in
    v

  let decode_bin_length =
    match Irmin.Type.(Size.of_encoding (pair H.t t)) with
    | Dynamic f -> f
    | _ -> assert false
end

module I = Irmin_pack_unix.Index
module Index = Irmin_pack_unix.Index.Make (Schema.Hash)
module Key = Irmin_pack.Pack_key.Make (Schema.Hash)
module P = Irmin_pack_unix.Pack_store.Maker (Index) (Schema.Hash)
module Pack = P.Make (Contents)

module Branch =
  Irmin_pack_unix.Atomic_write.Make_persistent
    (Irmin.Branch.String)
    (Irmin_pack.Atomic_write.Value.Of_hash (Schema.Hash))

module Make_context (Config : sig
  val root : string
end) =
struct
  let fresh_name =
    let c = ref 0 in
    fun object_type ->
      incr c;
      let name = Filename.concat Config.root ("pack_" ^ string_of_int !c) in
      [%logs.info "Constructing %s context object: %s" object_type name];
      name

  type d = { dict : Dict.t; clone : readonly:bool -> Dict.t }

  let get_dict () =
    let name = fresh_name "dict" in
    let dict = Dict.v ~fresh:true name in
    let clone ~readonly = Dict.v ~fresh:false ~readonly name in
    { dict; clone }

  type t = {
    index : Index.t;
    pack : read Pack.t;
    clone_pack : readonly:bool -> read Pack.t Lwt.t;
    clone_index_pack : readonly:bool -> (Index.t * read Pack.t) Lwt.t;
  }

  let log_size = 10_000_000

  let get_pack ?(lru_size = 0) () =
    let name = fresh_name "dict" in
    let f = ref (fun () -> ()) in
    let index =
      Index.v ~flush_callback:(fun () -> !f ()) ~log_size ~fresh:true name
    in
    let indexing_strategy = Irmin_pack.Indexing_strategy.always in
    let+ pack = Pack.v ~fresh:true ~lru_size ~index ~indexing_strategy name in
    (f := fun () -> Pack.flush ~index:false pack);
    let clone_pack ~readonly =
      Pack.v ~lru_size ~fresh:false ~readonly ~index ~indexing_strategy name
    in
    let clone_index_pack ~readonly =
      let index = Index.v ~log_size ~fresh:false ~readonly name in
      let+ pack =
        Pack.v ~lru_size ~fresh:false ~readonly ~index ~indexing_strategy name
      in
      (index, pack)
    in
    { index; pack; clone_pack; clone_index_pack }

  let close index pack =
    Index.close index;
    Pack.close pack
end

module Alcotest = struct
  include Alcotest

  let int63 = testable Int63.pp Int63.equal

  (** TODO: upstream this to Alcotest *)
  let check_raises_lwt msg exn (type a) (f : unit -> a Lwt.t) =
    Lwt.catch
      (fun x ->
        let* (_ : a) = f x in
        Alcotest.failf
          "Fail %s: expected function to raise %s, but it returned instead." msg
          (Printexc.to_string exn))
      (function
        | e when e = exn -> Lwt.return_unit
        | e ->
            Alcotest.failf
              "Fail %s: expected function to raise %s, but it raised %s \
               instead."
              msg (Printexc.to_string exn) (Printexc.to_string e))

  let testable_repr t =
    Alcotest.testable (Irmin.Type.pp t) Irmin.Type.(unstage (equal t))

  let check_repr ?pos t = Alcotest.check ?pos (testable_repr t)
end

module Filename = struct
  include Filename

  (** Extraction from OCaml for pre-4.10 compatibility *)

  let generic_quote quotequote s =
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\'';
    for i = 0 to l - 1 do
      if s.[i] = '\'' then Buffer.add_string b quotequote
      else Buffer.add_char b s.[i]
    done;
    Buffer.add_char b '\'';
    Buffer.contents b

  let quote_command =
    match Sys.os_type with
    | "Win32" ->
        let quote s =
          let l = String.length s in
          let b = Buffer.create (l + 20) in
          Buffer.add_char b '\"';
          let rec loop i =
            if i = l then Buffer.add_char b '\"'
            else
              match s.[i] with
              | '\"' -> loop_bs 0 i
              | '\\' -> loop_bs 0 i
              | c ->
                  Buffer.add_char b c;
                  loop (i + 1)
          and loop_bs n i =
            if i = l then (
              Buffer.add_char b '\"';
              add_bs n)
            else
              match s.[i] with
              | '\"' ->
                  add_bs ((2 * n) + 1);
                  Buffer.add_char b '\"';
                  loop (i + 1)
              | '\\' -> loop_bs (n + 1) (i + 1)
              | _ ->
                  add_bs n;
                  loop i
          and add_bs n =
            for _j = 1 to n do
              Buffer.add_char b '\\'
            done
          in
          loop 0;
          Buffer.contents b
        in
        let quote_cmd s =
          let b = Buffer.create (String.length s + 20) in
          String.iter
            (fun c ->
              match c with
              | '(' | ')' | '!' | '^' | '%' | '\"' | '<' | '>' | '&' | '|' ->
                  Buffer.add_char b '^';
                  Buffer.add_char b c
              | _ -> Buffer.add_char b c)
            s;
          Buffer.contents b
        in
        let quote_cmd_filename f =
          if String.contains f '\"' || String.contains f '%' then
            failwith ("Filename.quote_command: bad file name " ^ f)
          else if String.contains f ' ' then "\"" ^ f ^ "\""
          else f
        in
        fun cmd ?stdin ?stdout ?stderr args ->
          String.concat ""
            [
              "\"";
              quote_cmd_filename cmd;
              " ";
              quote_cmd (String.concat " " (List.map quote args));
              (match stdin with
              | None -> ""
              | Some f -> " <" ^ quote_cmd_filename f);
              (match stdout with
              | None -> ""
              | Some f -> " >" ^ quote_cmd_filename f);
              (match stderr with
              | None -> ""
              | Some f ->
                  if stderr = stdout then " 2>&1"
                  else " 2>" ^ quote_cmd_filename f);
              "\"";
            ]
    | _ -> (
        let quote = generic_quote "'\\''" in
        fun cmd ?stdin ?stdout ?stderr args ->
          String.concat " " (List.map quote (cmd :: args))
          ^ (match stdin with None -> "" | Some f -> " <" ^ quote f)
          ^ (match stdout with None -> "" | Some f -> " >" ^ quote f)
          ^
          match stderr with
          | None -> ""
          | Some f -> if stderr = stdout then " 2>&1" else " 2>" ^ quote f)
end

(** Exec a command, and return [Ok ()] or [Error n] if return code is n <> 0 *)
let exec_cmd cmd =
  [%logs.info "exec: %s" cmd];
  match Sys.command cmd with 0 -> Ok () | n -> Error n
