open Lwt.Infix
module Store = Irmin_pack.KV (Irmin.Contents.String)

let ignore_srcs src =
  List.mem (Logs.Src.name src)
    [ "git.inflater.decoder";
      "git.deflater.encoder";
      "git.encoder";
      "git.decoder";
      "git.loose";
      "git.store";
      "cohttp.lwt.io"
    ]

let reporter ?(prefix = "") () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Unix.gettimeofday () in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    if ignore_srcs src then Format.ikfprintf k ppf fmt
    else with_stamp header tags k fmt
  in
  { Logs.report }

let () =
  match Sys.getenv "BENCH_VERBOSE" with
  | exception Not_found -> ()
  | "0" | "" -> ()
  | "1" | "true" ->
      Logs.set_level (Some Logs.Debug);
      Logs.set_reporter (reporter ())
  | s -> Fmt.failwith "%s: invalid BENCH_VERBOSE value, use 1" s

let info () = Irmin.Info.v ~date:0L ~author:"author" "commit message"

let times ~n ~init f =
  let rec go i k =
    if i = 0 then k init else go (i - 1) (fun r -> f i r >>= k)
  in
  go n Lwt.return

let run ~path ~ncommits =
  let config = Irmin_pack.config ~fresh:false (Fpath.to_string path) in
  let tree = Store.Tree.empty in
  Store.Repo.v config >>= Store.master >>= fun t ->
  times ~n:ncommits ~init:tree (fun i tree ->
      Store.Tree.add tree [ string_of_int i ] "contents" >>= fun tree ->
      Store.set_tree_exn t ~info [] tree >>= fun () -> Lwt.return tree )
  >>= fun _ -> Lwt_io.printl "ok"

let main ~ncommits =
  Bos.OS.Dir.with_tmp "irmin%s"
    (fun path () -> Lwt_main.run (run ~path ~ncommits))
    ()
  |> Rresult.R.failwith_error_msg

let () =
  match Sys.argv with
  | [| _ |] -> main ~ncommits:1000
  | [| _; ncommits_str |] ->
      let ncommits = int_of_string ncommits_str in
      main ~ncommits
  | _ -> assert false
