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

module StringSet = struct
  include Set.Make(String)
  let of_list = List.fold_left (fun s x -> add x s) empty
end

let fmt t x = Printf.ksprintf (fun str -> t str) x

module File = struct

  type t =
    | String of string
    | Set of StringSet.t

  module T = Tc.Pair(Tc.String)(Tc.List(Tc.String))

  let of_t = function
    | String s -> ("string", [s])
    | Set s    -> ("set"   , StringSet.elements s)

  let to_t = function
    | "string", [s] -> String s
    | "set"   , s   -> Set (StringSet.of_list s)
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

end

module Contents = struct
  include File
  module Path = Irmin.Path.String_list

  let (++) = StringSet.union
  let (--) = StringSet.diff

  let merge _path ~old t1 t2 =
    match t1, t2 with
    | Set s1, Set s2 ->
      let old () =
        old () >>| function
        | Some (Set s) -> ok (Some s)
        | None         -> ok (Some StringSet.empty)
        | _ -> conflict "unmergeable set ancestor"
      in
      Irmin.Merge.set (module StringSet) ~old s1 s2 >>| fun s3 ->
      ok (Set s3)
    | String x1, String x2 ->
      let old () =
        old () >>| function
        | Some (String x) -> ok (Some x)
        | None            -> ok None
        | _ -> conflict "unmergable string ancestor"
      in
      Irmin.Merge.string ~old x1 x2 >>| fun x3 ->
      ok (String x3)
    | _ -> conflict "unmergeable contents"

  let merge path = Irmin.Merge.option (module File) (merge path)

end

module Store = Irmin.Basic(Irmin_git.FS)(Contents)
module View = Irmin.View(Store)

module Contact = struct

  type t = {
    id    : string;
    name  : string;
    phones: StringSet.t;
  }

  let view_of_t t =
    let name = Contents.String t.name in
    let phones = Contents.Set t.phones in
    View.empty () >>= fun v ->
    View.update v [t.id; "name"  ] name >>= fun () ->
    View.update v [t.id; "phones"] phones >>= fun () ->
    return v

  let t_of_view id v =
    View.read_exn v ["name"  ] >>= fun name ->
    View.read_exn v ["phones"] >>= fun phones ->
    let name = match name with
      | Contents.String s -> s
      | _                 -> failwith "name" in
    let phones = match phones with
      | Contents.Set s -> s
      | _              -> failwith "phones" in
    return { id; name; phones }

  let add t contact =
    view_of_t contact >>= fun view ->
    View.merge_path_exn (t "ContactStore.add") ["contacts"] view

  let add_phone contact phone =
    let phones = StringSet.add phone contact.phones in
    { contact with phones }

  let update_name contact name =
    { contact with name }

  let list t =
    let t = t "Contact.list" in
    Store.list t ["contacts"] >>= fun paths ->
    Lwt_list.map_s (fun path ->
        View.of_path t path >>= fun view ->
        let id = List.hd (List.rev path) in
        t_of_view id view
      ) paths

end

let jean = {
  Contact.id = "jean@dupont.fr";
  name       = "Jean Dupont";
  phones     = StringSet.of_list [ "+33 123456789" ]
}

let jane = {
  Contact.id = "jane@doo.com";
  name       = "Jane Doo";
  phones     = StringSet.empty;
}

let error () =
  Printf.eprintf "usage: contact init \n\
                 \       contact import <path>\n";
  exit 1

let main () =

  let argc = Array.length Sys.argv in
  if argc < 2  then error ();

  match Sys.argv.(1) with
  | "init" ->

    let local = Irmin_git.config () in

    Store.create local task >>= fun t ->
    Contact.add t jean >>= fun () ->
    Contact.add t jane   >>= fun () ->

    Store.clone_force task (t "Cloning test") "test" >>= fun test ->

    let jane1 =
      let x = Contact.add_phone jane "+44 12345" in
      let x = Contact.add_phone x "+44 45678" in
      x
    in
    let jane2 = Contact.add_phone jane "+33 123456" in

    Contact.add test jane1 >>= fun () ->
    Contact.add t jane2    >>= fun () ->

    Store.merge_exn "Merging test into the main branch" test ~into:t >>= fun () ->

    let jean = Contact.update_name jean "Jean Dupont" in
    Contact.add t jean >>= fun () ->

    return_unit

  | "import" ->

    if argc <> 3 then error ();
    let path = Sys.argv.(2) in

    let local = Irmin_git.config () in
    let remote = Irmin_git.config ~root:path () in

    Store.create local task  >>= fun local    ->
    Store.create remote task >>= fun remote   ->
    Contact.list remote >>= fun contacts ->
    Lwt_list.iter_p (Contact.add local) contacts

  | _ -> error ()

let () =
  Lwt_unix.run (main ())
