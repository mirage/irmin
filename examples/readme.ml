(* N.B. This excerpt is extracted from project README. Any changes made here
 * should be mirrored there. *)

open Lwt.Infix

(* Irmin store with string contents *)
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

(* Database configuration *)
let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

(* Commit author *)
let author = "Example <example@example.com>"

(* Commit information *)
let info fmt = Irmin_unix.info ~author fmt

let main =
  (* Open the repo *)
  Store.Repo.v config >>= (* Load the master branch *)
                          Store.master >>= fun t ->
  (* Set key "foo/bar" to "testing 123" *)
  Store.set_exn t ~info:(info "Updating foo/bar") [ "foo"; "bar" ]
    "testing 123"
  >>= fun () ->
  (* Get key "foo/bar" and print it to stdout *)
  Store.get t [ "foo"; "bar" ] >|= fun x -> Printf.printf "foo/bar => '%s'\n" x

(* Run the program *)
let () = Lwt_main.run main
