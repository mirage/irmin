open Lwt
open Irmin.Merge.OP
open Irmin_unix

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

module StringSet = Set.Make(String)
let set_of_list s =
  List.fold_left (fun s x -> StringSet.add x s) StringSet.empty s

module Contents = struct

  type t =
    | String of string
    | Set of StringSet.t

  module T = Tc.Pair(Tc.String)(Tc.List(Tc.String))

  let of_t = function
    | String s -> ("string", [s])
    | Set s    -> ("set"   , StringSet.elements s)

  let to_t = function
    | "string", [s] -> String s
    | "set"   , s   -> Set (set_of_list s)
    | _ -> failwith "Contents"

  let t = Tc.biject (module T) to_t of_t

  let compare = Tc.compare t
  let hash = Tc.hash t
  let equal = Tc.equal t
  let to_json = Tc.to_json t
  let of_json = Tc.of_json t
  let size_of = Tc.size_of t
  let write = Tc.write t
  let read = Tc.read t

  let (++) = StringSet.union
  let (--) = StringSet.diff

  let merge ~old t1 t2 =
    match old, t1, t2 with
    | Set old, Set s1, Set s2 ->
      Irmin.Merge.set (module StringSet) ~old s1 s2 >>| fun s3 ->
      ok (Set s3)
    | String old, String x1, String x2 ->
      Irmin.Merge.string ~old x1 x2 >>| fun x3 ->
      ok (String x3)
    | _ -> conflict "unmergeable contents"

end

module Store = Irmin.Default(Irmin_git.FS)(Contents)
module View = Irmin.View(Store)

module Contact = struct

  type t = {
    id    : string;
    name  : string;
    phones: StringSet.t;
  }

  let view_of_t config t =
    let name = Contents.String t.name in
    let phones = Contents.Set t.phones in
    View.create config task >>= fun view ->
    View.update view [t.id; "name"  ] name   >>= fun () ->
    View.update view [t.id; "phones"] phones >>= fun () ->
    return view

  let t_of_view id view =
    View.read_exn view ["name"  ] >>= fun name ->
    View.read_exn view ["phones"] >>= fun phones ->
    let name = match name with
      | Contents.String s -> s
      | _                 -> failwith "name" in
    let phones = match phones with
      | Contents.Set s -> s
      | _              -> failwith "phones" in
    return { id; name; phones }

end

module ContactStore (Store: S) = struct

  open Contact

  let add t contact =
    Contact.view_of_t contact >>= fun view ->
    Store.View.merge_path_exn t ["contacts"] view

  let add_phone contact phone =
    let phones = String.Set.add contact.phones phone in
    { contact with phones }

  let update_name contact name =
    { contact with name }

  let list t =
    Store.list t [["contacts"]] >>= fun paths ->
    Lwt_list.map_s (fun path ->
        Store.View.of_path t path >>= fun view ->
        let id = List.hd_exn (List.rev path) in
        t_of_view id view
      ) paths

end

let thomas = {
  Contact.id = "tg364";
  name       = "Thomas Gazagnaire";
  phones     = String.Set.of_list [ "+33 677891037"; "+44 7712345655" ]
}

let anil = {
  Contact.id = "avsm2";
  name       = "Anil";
  phones     = String.Set.empty;
}

let error () =
  eprintf "usage: contact init \n\
          \       contact import <path>\n";
  exit 1

module Git (C: sig val root: string option end) = struct
  module G = IrminGit.FS(struct
      let root = C.root
      let bare = true
    end)
  include G.Make(IrminKey.SHA1)(Contents)(IrminTag.String)
end

let main () =

  let argc = Array.length Sys.argv in
  if argc < 2  then error ();

  match Sys.argv.(1) with
  | "init" ->

    let module Local = Git(struct let root = None end) in
    let module CS    = ContactStore (Local) in

    Local.create () >>= fun t ->
    CS.add t thomas >>= fun () ->
    CS.add t anil   >>= fun () ->

    Local.clone_force t "test" >>= fun test ->

    let anil_test = CS.add_phone anil      "+44 12345" in
    let anil_test = CS.add_phone anil_test "+44 45678" in
    let anil_t    = CS.add_phone anil      "+33 123456" in

    CS.add test anil_test >>= fun () ->
    CS.add t anil_t       >>= fun () ->

    Local.merge_exn t (Local.branch_exn test) >>= fun () ->

    let thomas = CS.update_name thomas "T. Gazagnaire" in
    CS.add t thomas >>= fun () ->

    return_unit

  | "import" ->

    if argc <> 3 then error ();
    let path = Sys.argv.(2) in

    let module Local  = Git(struct let root = None end) in
    let module Remote = Git(struct let root = Some path end) in

    let module LocalCS  = ContactStore (Local) in
    let module RemoteCS = ContactStore (Remote) in

    Local.create ()      >>= fun local    ->
    Remote.create ()     >>= fun remote   ->
    RemoteCS.list remote >>= fun contacts ->
    Lwt_list.iter_p (LocalCS.add local) contacts

  | _ -> error ()

let () =
  Lwt_unix.run (main ())
