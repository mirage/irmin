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
  let magic = 'B'
  let encode_triple = Irmin.Type.(unstage (encode_bin (triple H.t char t)))
  let decode_triple = Irmin.Type.(unstage (decode_bin (triple H.t char t)))
  let length_header = Fun.const (Some `Varint)
  let encode_bin ~dict:_ ~offset_of_key:_ k x = encode_triple (k, magic, x)

  let decode_bin ~dict:_ ~key_of_offset:_ ~key_of_hash:_ x pos_ref =
    let _, _, v = decode_triple x pos_ref in
    v

  let decode_bin_length =
    match Irmin.Type.(Size.of_encoding (triple H.t char t)) with
    | Dynamic f -> f
    | _ -> assert false
end

module I = Irmin_pack_unix.Index
module Index = Irmin_pack_unix.Index.Make (Schema.Hash)
module Key = Irmin_pack.Pack_key.Make (Schema.Hash)
module Io = Irmin_pack_unix.Io.Unix
module Errs = Irmin_pack_unix.Io_errors.Make (Io)
module Control = Irmin_pack_unix.Control_file.Make (Io)
module Aof = Irmin_pack_unix.Append_only_file.Make (Io)

module File_manager =
  Irmin_pack_unix.File_manager.Make (Control) (Aof) (Aof) (Index) (Errs)

module Dict = Irmin_pack_unix.Dict.Make (File_manager)
module Dispatcher = Irmin_pack_unix.Dispatcher.Make (File_manager)

module Pack =
  Irmin_pack_unix.Pack_store.Make (File_manager) (Dict) (Dispatcher)
    (Schema.Hash)
    (Contents)
    (Errs)

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

  let mkdir_dash_p dirname = Irmin_pack_unix.Io_legacy.Unix.mkdir dirname

  type d = { name : string; fm : File_manager.t; dict : Dict.t }

  (* TODO : test the indexing_strategy minimal. *)
  let config ~readonly ~fresh name =
    Irmin_pack.Conf.init ~fresh ~readonly
      ~indexing_strategy:Irmin_pack.Indexing_strategy.always ~lru_size:0 name

  (* TODO : remove duplication with irmin_pack/ext.ml *)
  let get_fm config =
    let readonly = Irmin_pack.Conf.readonly config in
    if readonly then File_manager.open_ro config |> Errs.raise_if_error
    else
      let fresh = Irmin_pack.Conf.fresh config in
      if fresh then (
        let root = Irmin_pack.Conf.root config in
        mkdir_dash_p root;
        File_manager.create_rw ~overwrite:true config |> Errs.raise_if_error)
      else File_manager.open_rw config |> Errs.raise_if_error

  let get_dict ?name ~readonly ~fresh () =
    let name = Option.value name ~default:(fresh_name "dict") in
    let fm = config ~readonly ~fresh name |> get_fm in
    let dict = Dict.v fm |> Errs.raise_if_error in
    { name; dict; fm }

  let close_dict d = File_manager.close d.fm |> Errs.raise_if_error

  type t = {
    name : string;
    fm : File_manager.t;
    index : Index.t;
    pack : read Pack.t;
    dict : Pack.dict;
  }

  let create ~readonly ~fresh name =
    let f = ref (fun () -> ()) in
    let config = config ~readonly ~fresh name in
    let fm = get_fm config in
    let dispatcher = Dispatcher.v ~root:name fm |> Errs.raise_if_error in
    (* open the index created by the fm. *)
    let index = File_manager.index fm in
    let dict = Dict.v fm |> Errs.raise_if_error in
    let pack = Pack.v ~config ~fm ~dict ~dispatcher in
    (f := fun () -> File_manager.flush fm |> Errs.raise_if_error);
    { name; index; pack; dict; fm } |> Lwt.return

  let get_rw_pack () =
    let name = fresh_name "" in
    create ~readonly:false ~fresh:true name

  let get_ro_pack name = create ~readonly:true ~fresh:false name
  let reopen_rw name = create ~readonly:false ~fresh:false name

  let close_pack t =
    Index.close_exn t.index;
    File_manager.close t.fm |> Errs.raise_if_error;
    (* closes pack and dict *)
    Lwt.return_unit
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
  let kind = testable_repr Irmin_pack.Pack_value.Kind.t
  let hash = testable_repr Schema.Hash.t
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

let rec repeat = function
  | 0 -> fun _f x -> x
  | n -> fun f x -> f (repeat (n - 1) f x)

(** The current working directory depends on whether the test binary is directly
    run or is triggered with [dune exec], [dune runtest]. We normalise by
    switching to the project root first. *)
let goto_project_root () =
  let cwd = Fpath.v (Sys.getcwd ()) in
  match cwd |> Fpath.segs |> List.rev with
  | "irmin-pack" :: "test" :: "default" :: "_build" :: _ ->
      let root = cwd |> repeat 4 Fpath.parent in
      Unix.chdir (Fpath.to_string root)
  | _ -> ()

let setup_test_env ~root_archive ~root_local_build =
  goto_project_root ();
  rm_dir root_local_build;
  let cmd =
    Filename.quote_command "cp" [ "-R"; "-p"; root_archive; root_local_build ]
  in
  exec_cmd cmd |> function
  | Ok () -> ()
  | Error n ->
      Fmt.failwith
        "Failed to set up the test environment: command `%s' exited with \
         non-zero exit code %d"
        cmd n
