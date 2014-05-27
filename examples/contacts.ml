open Lwt
open Core_kernel.Std
open IrminMerge.OP

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

module Contents = struct

  type elt =
    | String of string
    | Set of String.Set.t
  with bin_io, compare, sexp

  (* Glue code *)
  module X = IrminIdent.Make(struct
      type nonrec t = elt with compare, sexp
    end)
  include X

  let to_string = function
    | String s -> sprintf "%s\n" s
    | Set  s   -> sprintf "List:\n%s\n" (String.concat ~sep:"\n" (String.Set.to_list s))

  let of_string str =
    match String.split str ~on:'\n' with
    | "List:" :: l ->
      let l = List.filter ~f:(fun x -> String.(x <> "")) l in
      Set (String.Set.of_list l)
    | _            -> String str

  let (++) = String.Set.union
  let (--) = String.Set.diff

  let merge_t ~old t1 t2 =
    match old, t1, t2 with

    | Set old, Set s1, Set s2 ->
      let add = (s1 -- old) ++ (s2 -- old) in
      let del = (old -- s1) ++ (old -- s2) in
      ok (Set (add ++ old -- del))

    | String old, String x1, String x2 ->
      if String.(old = x1) then ok (String x2)
      else if String.(old = x2) then ok (String x1)
      else if String.(x1 = x2) then ok (String x1)
      else conflict "Not mergeable string (%s / %s / %s)" old x1 x2

    | _ -> conflict "Not mergeable contents"

  let merge = IrminMerge.create' (module X) merge_t

end

module View = IrminView.Make(IrminKey.SHA1)(Contents)

module Contact = struct

  type t = {
    id    : string;
    name  : string;
    phones: String.Set.t;
  }

  let view_of_t t =
    let name = Contents.String t.name in
    let phones = Contents.Set t.phones in
    View.create () >>= fun view ->
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

module type S = Irmin.S with type Block.key = IrminKey.SHA1.t
                         and type value = Contents.t
                         and type branch = IrminTag.String.t

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
  module LocalConfig = struct
    let root = C.root
    module Store = Git_fs
    let bare = true
    let disk = true
  end
  module G = IrminGit.Make(LocalConfig)
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

    Local.clone_force t "refs/heads/test" >>= fun test ->

    let anil_test = CS.add_phone anil      "+44 12345" in
    let anil_test = CS.add_phone anil_test "+44 45678" in
    let anil_t    = CS.add_phone anil      "+33 123456" in

    CS.add test anil_test >>= fun () ->
    CS.add t anil_t       >>= fun () ->

    Local.merge_exn t (Local.branch test) >>= fun () ->

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
