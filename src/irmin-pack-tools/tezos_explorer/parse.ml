open Import
open Files
module Files = Make (Irmin_tezos.Conf) (Irmin_tezos.Schema)

type ctx = { off : Int63.t; info : info }
and info = { commits : int list; contents : int list; inodes : int list }

let empty_info () = { commits = []; contents = []; inodes = [] }

type info_ring = {
  commit : int Ring.t;
  contents : int Ring.t;
  inode : int Ring.t;
}

let info_ring () =
  { commit = Ring.make 1000; contents = Ring.make 1000; inode = Ring.make 1000 }

let ctx_buffer = Bytes.create (4096 * 4096)

let dump_ctx fd ctx =
  let idx = ref 0 in
  let f s =
    String.iteri (fun i c -> Bytes.set ctx_buffer (!idx + i) c) s;
    idx := !idx + String.length s
  in
  let flag =
    List.length ctx.info.commits
    + Int.shift_left (List.length ctx.info.contents) 2
    + Int.shift_left (List.length ctx.info.inodes) 4
  in
  Varint.encode_bin flag f;
  List.iter (fun v -> Varint.encode_bin v f) ctx.info.commits;
  List.iter (fun v -> Varint.encode_bin v f) ctx.info.contents;
  List.iter (fun v -> Varint.encode_bin v f) ctx.info.inodes;
  let _ = Unix.write fd ctx_buffer 0 !idx in
  (!idx, ctx.off)

let dump_idx fd i i2 off =
  let idx = ref 0 in
  let f s =
    String.iteri (fun i c -> Bytes.set ctx_buffer (!idx + i) c) s;
    idx := !idx + String.length s
  in
  Varint.encode_bin i f;
  Varint.encode_bin i2 f;
  Varint.encode_bin off f;
  let _ = Unix.write fd ctx_buffer 0 !idx in
  ()

let dump_idxs fd n is is2 =
  let idx = ref 0 in
  let f s =
    String.iteri (fun i c -> Bytes.set ctx_buffer (!idx + i) c) s;
    idx := !idx + String.length s
  in
  Varint.encode_bin (n + 1) f;
  let _ = Unix.write fd ctx_buffer 0 !idx in
  let off = List.fold_left (fun acc (i, _) -> i + acc) 0 is2 in
  let _ =
    List.fold_left2
      (fun (i, i2) (i', off) (i2', _) ->
        dump_idx fd i (i2 - i2') (Int63.to_int off);
        (i + i', i2 - i2'))
      (0, off) is is2
  in
  ()

let get_values r = List.filter_map (Ring.get r) [ 1; 10; 1000 ]

let main store_path info_last_path info_next_path idx_path =
  Eio.Switch.run @@ fun sw ->
  let conf = Irmin_pack.Conf.init store_path in
  match Files.File_manager.open_ro ~sw conf with
  | Error exn -> Fmt.pr "%a\n%!" (Irmin.Type.pp Files.Errs.t) exn
  | Ok fm ->
      let info_fd =
        Unix.openfile info_last_path Unix.[ O_RDWR; O_CREAT; O_TRUNC ] 0o644
      in
      let idxs = ref [] in
      let offsets = ref [] in
      let i = ref (-1) in
      let last_info = ref (empty_info ()) in
      let r = info_ring () in
      let on_entry off entry =
        incr i;
        let ctx = { off; info = !last_info } in
        (match entry with
        | `Commit ->
            Ring.add r.commit !i;
            let commits = get_values r.commit in
            last_info := { !last_info with commits }
        | `Contents ->
            Ring.add r.contents !i;
            let contents = get_values r.contents in
            last_info := { !last_info with contents }
        | `Inode ->
            Ring.add r.inode !i;
            let inodes = get_values r.inode in
            last_info := { !last_info with inodes });
        let idx = dump_ctx info_fd ctx in
        idxs := idx :: !idxs;
        offsets := (entry, off) :: !offsets
      in
      Files.iter_store fm ~on_entry;
      let entries = !i in
      Unix.close info_fd;
      let info_fd =
        Unix.openfile info_next_path Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
      in
      let idxs2 = ref [] in
      let i = ref (-1) in
      let last_info = ref (empty_info ()) in
      let r = info_ring () in
      let get_next (entry, off) =
        incr i;
        let ctx = { off; info = !last_info } in
        (match entry with
        | `Commit ->
            Ring.add r.commit (entries - !i);
            let commits = get_values r.commit in
            last_info := { !last_info with commits }
        | `Contents ->
            Ring.add r.contents (entries - !i);
            let contents = get_values r.contents in
            last_info := { !last_info with contents }
        | `Inode ->
            Ring.add r.inode (entries - !i);
            let inodes = get_values r.inode in
            last_info := { !last_info with inodes });
        let idx = dump_ctx info_fd ctx in
        idxs2 := idx :: !idxs2
      in
      List.iter get_next !offsets;
      let idx_fd =
        Unix.openfile idx_path Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
      in
      dump_idxs idx_fd entries (List.rev !idxs) !idxs2;
      Unix.close idx_fd

let main store_path info_last_path info_next_path index_path =
  main store_path info_last_path info_next_path index_path
