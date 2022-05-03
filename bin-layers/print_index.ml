(** A simple utility to provide a human-readable version of the data in the context index. *)

let path = Sys.argv |> Array.to_list |> List.tl |> function
  | [path] -> path
  | _ -> failwith "Usage: print_index.exe <path to index dir>"


module Pack_index = Irmin_pack_unix.Index

module Index_ = Pack_index.Make(Irmin_tezos.Schema.Hash)

(* FIXME log_size:0 in following? presumably log_size is only relevant if readonly is
   false? *)
let index : Index_.t = Index_.v ~readonly:true ~log_size:0 path

(* following just to get a value_t to use for pretty printing; FIXME better to expose
   value_t in index sig? *)
include struct

  module Int63 = struct
    include Optint.Int63

    let t = Irmin.Type.int63
  end

  type int63 = Int63.t [@@deriving irmin]

  type value = int63 * int * Irmin_pack.Pack_value.Kind.t[@@deriving irmin]
end


let pp_k = Repr.pp Index_.Key.t
let pp_v = Repr.pp value_t

let _ = 
  index |> Index_.iter (fun k v -> 
      Format.printf "%a %a\n%!" pp_k k pp_v v)





