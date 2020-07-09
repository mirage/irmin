open Lwt.Infix
module Dict = Irmin_pack.Dict

let get = function Some x -> x | None -> Alcotest.fail "None"

let sha1 x = Irmin.Hash.SHA1.hash (fun f -> f x)

let rm_dir root =
  if Sys.file_exists root then (
    let cmd = Printf.sprintf "rm -rf %s" root in
    Logs.info (fun l -> l "exec: %s\n%!" cmd);
    let _ = Sys.command cmd in
    ())

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

  let get_pack () =
    let name = fresh_name "dict" in
    let f = ref (fun () -> ()) in
    let index =
      Index.v ~auto_flush_callback:(fun () -> !f ()) ~log_size ~fresh:true name
    in
    Pack.v ~fresh:true ~lru_size:0 ~index name >|= fun pack ->
    (f := fun () -> Pack.flush ~index:false pack);
    let clone_pack ~readonly = Pack.v ~fresh:false ~readonly ~index name in
    let clone_index_pack ~readonly =
      let index = Index.v ~log_size ~fresh:false ~readonly name in
      Pack.v ~fresh:false ~readonly ~index name >|= fun pack -> (index, pack)
    in
    { index; pack; clone_pack; clone_index_pack }

  let close index pack =
    Index.close index;
    Pack.close pack
end
