(* Races the global [cache : (string, _) Hashtbl.t] captured by
   [Irmin_mem.Read_only.v] in src/irmin/mem/irmin_mem.ml:44.

   N domains barrier-start, then each opens and closes a Repo.v
   repeatedly. On cold cache they all race on Hashtbl.find_opt/add;
   after that, set_exn on the shared repo races the mutable KMap field
   of the underlying Read_only instance. *)

module Store = Irmin_mem.KV.Make (Irmin.Contents.String)

let run ~env ~iter =
  let dmgr = Eio.Stdenv.domain_mgr env in
  let info () = Store.Info.v 0L in
  let worker id () =
    for i = 1 to iter do
      let repo = Store.Repo.v (Irmin_mem.config ()) in
      let main = Store.main repo in
      Store.set_exn ~info main [ Printf.sprintf "k-%d-%d" id i ] "v";
      Store.Repo.close repo
    done
  in
  let fns = List.init 4 (fun id -> worker id) in
  Stress_common.domains_run ~dmgr fns
