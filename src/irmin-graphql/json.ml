type t = Irmin.Contents.json

let of_string = Irmin.Type.of_string Irmin.Contents.Json_value.t
let to_string = Irmin.Type.to_string Irmin.Contents.Json_value.t

let rec find (j: t) keys: t option =
  match keys with
  | [] -> Some j
  | hd :: tl ->
    (match j with
    | `O obj ->
        (match List.assoc_opt hd obj with
        | Some v -> find v tl
        | None -> None)
    | _ -> None)

let find_exn (j: t) keys: t =
  match find j keys with
  | Some x -> x
  | None -> raise (Invalid_argument ("find_exn: " ^ String.concat ", " keys))

let get_string_exn: t -> string = function
  | `String s -> s
  | `Float f -> string_of_float f
  | `Bool b -> string_of_bool b
  | _ -> raise (Invalid_argument "get_string_exn")
