open Cmdliner

let default_uri = Uri.of_string "tcp://127.0.0.1:9181"

let uri : Uri.t option Cmdliner.Term.t =
  let doc =
    Arg.info ~docv:"URL" ~doc:"URI to connect to or listen on" [ "uri"; "u" ]
  in
  Term.(
    const (Option.map Uri.of_string)
    $ Arg.(value & opt (some string) None & doc))

let config_path : string option Cmdliner.Term.t =
  let doc = Arg.info ~docv:"PATH" ~doc:"Config path" [ "config" ] in
  Arg.(value & opt (some string) None & doc)

let codec =
  let doc =
    Arg.info ~docv:"CODEC" ~doc:"Encoding to use for messages" [ "codec" ]
  in
  let t = Arg.enum [ ("bin", `Bin); ("json", `Json) ] in
  Arg.(value & opt t `Bin doc)

module Conf = struct
  let spec = Irmin.Backend.Conf.Spec.v "server"

  module Key = struct
    let uri =
      Irmin.Backend.Conf.key ~spec "uri" Irmin.Backend.Conf.uri default_uri
  end

  let add_uri config u = Irmin.Backend.Conf.add config Key.uri u

  let v config uri =
    let spec' = Irmin.Backend.Conf.spec config in
    let spec = Irmin.Backend.Conf.Spec.join spec' [ spec ] in
    let config = Irmin.Backend.Conf.with_spec config spec in
    match uri with Some uri -> add_uri config uri | None -> config
end
