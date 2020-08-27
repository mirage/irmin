open Lwt.Infix
module Dict = Irmin_pack.Dict

let ( let* ) x f = Lwt.bind x f

let get = function Some x -> x | None -> Alcotest.fail "None"

let sha1 x = Irmin.Hash.SHA1.hash (fun f -> f x)

let rm_dir root =
  if Sys.file_exists root then (
    let cmd = Printf.sprintf "rm -rf %s" root in
    Logs.info (fun l -> l "exec: %s\n%!" cmd);
    let _ = Sys.command cmd in
    ())

let index_log_size = Some 1_000

module Conf = struct
  let entries = 32

  let stable_hash = 256
end

module S = struct
  include Irmin.Contents.String

  let magic _ = 'S'

  module H = Irmin.Hash.Typed (Irmin.Hash.SHA1) (Irmin.Contents.String)

  let hash = H.hash

  let encode_pair = Irmin.Type.(unstage (encode_bin (pair H.t t)))

  let decode_pair = Irmin.Type.(unstage (decode_bin (pair H.t t)))

  let encode_bin ~dict:_ ~offset:_ x k = encode_pair (k, x)

  let decode_bin ~dict:_ ~hash:_ x off =
    let _, (_, v) = decode_pair x off in
    v
end

module H = Irmin.Hash.SHA1
module I = Index
module Index = Irmin_pack.Index.Make (H)
module P = Irmin_pack.Pack.File (Index) (H)
module Pack = P.Make (S)
module Branch = Irmin_pack.Atomic_write (Irmin.Branch.String) (H)

module Make_context (Config : sig
  val root : string
end) =
struct
  let fresh_name =
    let c = ref 0 in
    fun object_type ->
      incr c;
      let name = Filename.concat Config.root ("pack_" ^ string_of_int !c) in
      Logs.info (fun m ->
          m "Constructing %s context object: %s" object_type name);
      name

  type d = { dict : Dict.t; clone : readonly:bool -> Dict.t }

  let get_dict () =
    let name = fresh_name "dict" in
    let dict = Dict.v ~fresh:true name in
    let clone ~readonly = Dict.v ~fresh:false ~readonly name in
    { dict; clone }

  type t = {
    index : Index.t;
    pack : [ `Read ] Pack.t;
    clone_pack : readonly:bool -> [ `Read ] Pack.t Lwt.t;
    clone_index_pack : readonly:bool -> (Index.t * [ `Read ] Pack.t) Lwt.t;
  }

  let log_size = 10_000_000

  let get_pack ?(lru_size = 0) () =
    let name = fresh_name "dict" in
    let f = ref (fun () -> ()) in
    let index =
      Index.v ~flush_callback:(fun () -> !f ()) ~log_size ~fresh:true name
    in
    Pack.v ~fresh:true ~lru_size ~index name >|= fun pack ->
    (f := fun () -> Pack.flush ~index:false pack);
    let clone_pack ~readonly =
      Pack.v ~lru_size ~fresh:false ~readonly ~index name
    in
    let clone_index_pack ~readonly =
      let index = Index.v ~log_size ~fresh:false ~readonly name in
      Pack.v ~lru_size ~fresh:false ~readonly ~index name >|= fun pack ->
      (index, pack)
    in
    { index; pack; clone_pack; clone_index_pack }

  let close index pack =
    Index.close index;
    Pack.close pack
end

module Alcotest = struct
  include Alcotest

  (** TODO: upstream this to Alcotest *)
  let check_raises_lwt msg exn (type a) (f : unit -> a Lwt.t) =
    Lwt.catch
      (fun x ->
        f x >>= fun (_ : a) ->
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
