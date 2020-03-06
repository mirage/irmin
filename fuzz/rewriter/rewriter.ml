(*
 * Rewrites the file at path [Sys.argv.(1)] by replacing:
 * - The [%impl_record n] extension point by a function which dynamically
     builds Irmin records with 1 up to [n] fields.
 * - The [%impl_variant n] extension point by a function which dynamically
     builds Irmin variants with 1 up to [n] cases.
 *)

let fmt = Printf.sprintf

let concat = String.concat

(** Generates the code for the [%impl_record n] extension point. *)
let impl_record n =
  let glue_cases cases =
    fmt
      {|
      fun n fs ->
        let w, i, d, g = (wrap, t_to_irmin, new_dyn_record, new_dyn_record_getter) in
        match fs with
        %s
        | _ ->
          failwith "The given TRecord has a number of fields outside of [|1; %d|]."
      |}
      (concat "\n" cases) n
  in

  let generate_case indices =
    let patterns = indices |> List.map (fun i -> fmt "(n%d, AT t%d)" i i) in
    let params = indices |> List.map (fun i -> fmt "v%d" i) in
    let values = indices |> List.map (fun i -> fmt "(n%d, w t%d v%d)" i i i) in
    let fields =
      indices
      |> List.map (fun i -> fmt "|+ T.field n%d (i t%d) (g n n%d t%d)" i i i i)
    in

    fmt
      {|
      | [ %s ] ->
        T.record n (fun %s -> d n [ %s ])
        %s
        |> T.sealr |}
      (concat "; " patterns) (concat " " params) (concat "; " values)
      (concat "\n" fields)
  in

  List.init n (fun i -> i + 1)
  |> List.map (fun length -> List.init length (fun i -> i + 1))
  |> List.map generate_case
  |> glue_cases

(** Generates the code for the [%impl_variant n] extension point. *)
let impl_variant n =
  let glue_cases cases =
    fmt
      {|
      fun n cs ->
        let w, u, i, eq = (wrap, unwrap, t_to_irmin, String.equal) in
        let ve vn =
          Fmt.failwith "%s" n vn
        in
        let ce cn =
          Fmt.failwith "%s" cn
        in
        match cs with
        %s
        | _ ->
          failwith "The given TVariant has a number of fields outside of [|1; %d|]."
      |}
      "Trying to access the wrong variant: wanted %s, got %s."
      "Trying to use an unknown case name: %s." (concat "\n" cases) n
  in

  let generate_case indices =
    let patterns =
      indices
      |> List.map (function
           | i, `Case0 -> fmt "(n%d, ACT Case0)" i
           | i, `Case1 -> fmt "(n%d, ACT (Case1 t%d))" i i)
    in
    let params = indices |> List.map (fun (i, _) -> fmt "c%d" i) in
    let inits =
      indices
      |> List.map (function
           | i, `Case0 -> fmt "| _, r, _ when eq r n%d -> c%d" i i
           | i, `Case1 -> fmt "| _, r, v when eq r n%d -> c%d (u t%d v)" i i i)
    in
    let cases =
      indices
      |> List.map (function
           | i, `Case0 -> fmt "|~ T.case0 n%d (n, n%d, VUnit ())" i i
           | i, `Case1 ->
               fmt "|~ T.case1 n%d (i t%d) (fun v -> (n, n%d, w t%d v))" i i i i)
    in

    fmt
      {|
      | [ %s ] ->
        T.variant n (fun %s -> function
          | vn, _, _ when not (eq n vn) -> ve vn
          %s
          | _, cn, _ -> ce cn)
        %s
        |> T.sealv |}
      (concat "; " patterns) (concat " " params) (concat "\n" inits)
      (concat "\n" cases)
  in

  (* Generate the i-th cartesian power l^i. *)
  let rec nth_product l = function
    | 0 -> [ [] ]
    | i ->
        nth_product l (i - 1)
        |> List.map (fun p -> List.map (fun e -> e :: p) l)
        |> List.flatten
  in

  List.init n (fun i -> i + 1)
  |> List.map (nth_product [ `Case0; `Case1 ])
  |> List.flatten
  |> List.map (List.mapi (fun i t -> (i + 1, t)))
  |> List.map generate_case
  |> glue_cases

let impl_record_regexp = Str.regexp "\\[% *impl_record \\([0-9]+\\)\\]"

let impl_variant_regexp = Str.regexp "\\[% *impl_variant \\([0-9]+\\)\\]"

let substitute regexp f m =
  Str.global_substitute regexp
    (fun _ -> Str.matched_group 1 m |> int_of_string |> f)
    m

let transform line =
  line
  |> substitute impl_record_regexp impl_record
  |> substitute impl_variant_regexp impl_variant

(** Transforms every line of the file passed as argv[1] using [transform]. *)
let () =
  let path = Sys.argv.(1) in
  let file = open_in path in
  try
    while true do
      input_line file |> transform |> print_endline
    done
  with End_of_file -> close_in file
