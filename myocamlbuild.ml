open Ocamlbuild_plugin
open Command

let crunch = (A"ocaml-crunch")

let static_files = [
  "d3.v3.min.js";
  "graphlib-dot.min.js";
  "scripts.js";
  "dagre-d3.min.js";
  "index.html";
  "style.css";
]

let static () =
  rule "Generate HTML static contents"
       ~prod:"src/http/irmin_http_static.ml"
       ~deps:(List.map (fun f -> "src/http/static/" ^ f) static_files)
       (fun _env _build ->
         let static_dir = A "src/http/static" in
         let static_ml  = A "src/http/irmin_http_static.ml" in
         Cmd(S[crunch; static_dir; A"-m"; A"plain"; A"-o"; static_ml]))

let () = dispatch (function After_rules -> static () | _ -> ())
