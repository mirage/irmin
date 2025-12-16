module Fs_store = Irmin_fs_unix.KV.Make(Irmin.Contents.Json_value)

let rec json_to_string = function
| `Null -> "null"
| `Bool b -> string_of_bool b
| `String s -> s
| `Float f -> string_of_float f
| `O u -> "{" ^ String.concat "; " (List.map (fun (k, v) -> "\"" ^ k ^ "\": " ^ json_to_string v) u) ^ "}"
| `A u -> "[" ^ String.concat "; " (List.map json_to_string u) ^ "]"

let get_tau env =
  let path = Eio.Path.(env#fs / "math") in
  let conf = Irmin_fs_unix.conf ~path ~clock:env#clock in
  let repo = Fs_store.Repo.v conf in
  let main = Fs_store.main repo in
  let tau = Fs_store.get main [ "tau" ] in
  assert (tau = `O ["val", `Float 6.28; ]);
  tau

let _ = Eio_main.run @@ fun env ->
  let tau = get_tau env in
  let str = json_to_string tau in
  Eio.Flow.copy_string str env#stdout;
