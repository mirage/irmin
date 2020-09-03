open Ppxlib

(*
 * - [%impl_record n] becomes becomes a function which builds Irmin records with 1 up to [n] fields.
 * - [%impl_variant n] becomes a function which builds Irmin variants with 1 up to [n] cases.
 *)

module type S = sig
  val impl_record : int -> expression

  val impl_variant : int -> expression
end

let ( >>| ) x f = List.map f x

let ( >>= ) x f = List.map f x |> List.flatten

module Located (A : Ast_builder.S) : S = struct
  open A

  let ev n i = evar (n ^ string_of_int i)

  let pv n i = pvar (n ^ string_of_int i)

  let plist : pattern list -> pattern =
   fun ps ->
    List.fold_right (fun hd tl -> [%pat? [%p hd] :: [%p tl]]) ps [%pat? []]

  let elist : expression list -> expression =
   fun es ->
    List.fold_right (fun hd tl -> [%expr [%e hd] :: [%e tl]]) es [%expr []]

  let efun ~(params : pattern list) : expression -> expression =
    List.fold_right
      (fun param body -> [%expr fun [%p param] -> [%e body]])
      params

  let error_case ~msg : case =
    case ~lhs:ppat_any ~guard:None ~rhs:[%expr failwith [%e estring msg]]

  (** Generates the code for the [%impl_record n] extension point. *)
  let impl_record n =
    let generate_case indices =
      let lhs =
        plist (indices >>| fun i -> [%pat? [%p pv "n" i], AT [%p pv "t" i]])
      in
      let wrap_params = efun ~params:(indices >>| pv "v") in
      let rhs =
        let apply_fields body =
          indices
          >>| (fun i body ->
                [%expr
                  [%e body]
                  |+ T.field [%e ev "n" i]
                       (t_to_irmin [%e ev "t" i])
                       (new_dyn_record_getter record_name [%e ev "n" i]
                          [%e ev "t" i])])
          |> List.fold_left ( |> ) body
        in
        let values =
          indices >>| fun i ->
          [%expr [%e ev "n" i], wrap [%e ev "t" i] [%e ev "v" i]]
        in
        [%expr
          [%e
            apply_fields
              [%expr
                T.record record_name
                  [%e
                    wrap_params
                      [%expr new_dyn_record record_name [%e elist values]]]]]
          |> T.sealr]
      in
      case ~lhs ~guard:None ~rhs
    in
    let cases =
      List.init n succ >>| (fun l -> List.init l succ) >>| generate_case
    in
    let error_case =
      error_case
        ~msg:
          (Format.sprintf
             "The given TRecord has a number of fields outside of [|1; %d|]" n)
    in
    [%expr
      fun record_name fs -> [%e pexp_match [%expr fs] (cases @ [ error_case ])]]

  let generate_case indices =
    let pattern : pattern =
      plist
        ( indices >>| fun (i, typ) ->
          match typ with
          | `Case0 -> [%pat? [%p pv "n" i], ACT Case0]
          | `Case1 -> [%pat? [%p pv "n" i], ACT (Case1 [%p pv "t" i])] )
    in
    let wrap_params : expression -> expression =
      indices
      >>| (fun (i, _) -> pv "c" i)
      |> List.fold_right (fun param body -> [%expr fun [%p param] -> [%e body]])
    in
    let inits : case list =
      let guard i = Some [%expr r = [%e ev "n" i]] in
      indices >>| function
      | i, `Case0 -> case ~lhs:[%pat? _, r, _] ~guard:(guard i) ~rhs:(ev "c" i)
      | i, `Case1 ->
          case
            ~lhs:[%pat? _, r, v]
            ~guard:(guard i)
            ~rhs:[%expr [%e ev "c" i] (unwrap [%e ev "t" i] v)]
    in
    let cases (body : expression) : expression =
      let case = function
        | i, `Case0 ->
            fun e ->
              [%expr
                [%e e]
                |~ T.case0 [%e ev "n" i] (variant_name, [%e ev "n" i], VUnit ())]
        | i, `Case1 ->
            fun e ->
              [%expr
                [%e e]
                |~ T.case1 [%e ev "n" i]
                     (t_to_irmin [%e ev "t" i])
                     (fun v ->
                       (variant_name, [%e ev "n" i], wrap [%e ev "t" i] v))]
      in
      indices >>| case |> List.fold_left ( |> ) body
    in
    let rhs =
      let destructor =
        [
          case
            ~lhs:[%pat? vn, _, _]
            ~guard:(Some [%expr not (variant_name = vn)])
            ~rhs:[%expr variant_error vn];
        ]
        @ inits
        @ [
            case
              ~lhs:[%pat? _, unmatched_case_name, _]
              ~guard:None
              ~rhs:[%expr case_error unmatched_case_name];
          ]
      in
      [%expr
        [%e
          cases
            [%expr
              T.variant variant_name [%e wrap_params (pexp_function destructor)]]]
        |> T.sealv]
    in
    case ~lhs:pattern ~guard:None ~rhs

  (** Generates the code for the [%impl_variant n] extension point. *)
  let impl_variant n =
    let error_case =
      error_case
        ~msg:
          (Format.sprintf
             "The given TVariant has a number of fields outside of [|1; %d|]."
             n)
    in
    (* Generate the i-th cartesian power l^i. *)
    let rec cart l = function
      | 0 -> [ [] ]
      | i ->
          cart l (i - 1) >>= fun p ->
          l >>| fun e -> e :: p
    in
    let cases =
      List.init n succ
      >>= cart [ `Case0; `Case1 ]
      >>| List.mapi (fun i t -> (succ i, t))
      >>| generate_case
    in
    [%expr
      fun variant_name cs ->
        let variant_error =
          Fmt.failwith "Trying to access the wrong variant: wanted %s, got %s"
            variant_name
        in
        let case_error =
          Fmt.failwith "Trying to use an unknown case name: %s"
        in
        [%e pexp_match [%expr cs] (cases @ [ error_case ])]]
end

let () =
  let extension f name =
    Extension.declare name Extension.Context.Expression
      Ast_pattern.(pstr (pstr_eval (eint __) nil ^:: nil))
      (fun ~loc ~path:_ ->
        let (module A) = Ast_builder.make loc in
        f (module Located (A) : S))
    |> Context_free.Rule.extension
  in
  Driver.register_transformation
    ~rules:
      [
        extension (fun (module L) -> L.impl_record) "impl_record";
        extension (fun (module L) -> L.impl_variant) "impl_variant";
      ]
    "alcotest.test"
