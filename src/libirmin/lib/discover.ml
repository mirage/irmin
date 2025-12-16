let quote s = "\"" ^ s ^ "\""

let () =
    let uname = Unix.open_process_args_in "uname" [| "uname"; "-s" |] in
    let finally () = In_channel.close uname in
    let output = Fun.protect ~finally @@ fun () ->
      In_channel.input_all uname |> String.trim
    in
    let flags = match output with
    | "Linux" -> ["-ccopt"; "-Wl,-znow"]
    | _ -> [] in
    let flags = List.map quote flags |> String.concat " " in
    Out_channel.with_open_text "link_flags.sexp" (fun oc ->
      Out_channel.output_char oc '(';
      Out_channel.output_string oc flags;
      Out_channel.output_char oc ')')
