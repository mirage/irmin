include Error_intf

let raise_error msg = raise (Error msg)

let unwrap prefix = function
  | Ok x -> x
  | Error (`Msg e) -> raise (Error (prefix ^ ": " ^ e))

let of_string s = `Msg s
let to_string = function `Msg s -> s

let () =
  Printexc.register_printer (function
    | Error msg -> Some msg
    | exn -> raise exn)
