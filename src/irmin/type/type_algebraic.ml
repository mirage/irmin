module Record (M : S.MONAD) = struct
  module Lens = Optics.Lens (M)

  open Type_core.Make (M)

  module Unwitnessed = struct
    type ('record, 'cons, 'lens, 'lens_nil) t = {
      name : string;
      cons : 'cons;
      fields : ('record, 'cons, 'lens, 'lens_nil) fields;
    }
  end

  type ('record, 'constr, 'remaining, 'lenses, 'lens_nil) open_record = {
    open_record :
      'hole. ('record, 'remaining, 'lens_nil, 'hole) fields ->
      (* Append the two lens lists at the type level *)
      ('record, 'constr, 'lenses, 'hole) Unwitnessed.t;
  }

  type nonrec ('a, 'b) field = ('a, 'b) field

  let field fname ftype fget = { fname; ftype; fget }

  let record :
      type r. string -> r -> ('a, r, r, 'lens_nil, 'lens_nil) open_record =
   fun name cons ->
    let open_record fields = Unwitnessed.{ name; cons; fields } in
    { open_record }

  let app :
      type r c ft rem lens lens_nil.
      (r, c, ft -> rem, lens, (r, ft) Lens.mono * lens_nil) open_record ->
      (r, ft) field ->
      (r, c, rem, lens, lens_nil) open_record =
   fun { open_record = previous } field ->
    let open_record' :
        type hole.
        (r, rem, lens_nil, hole) fields -> (r, c, lens, hole) Unwitnessed.t =
     fun fs -> previous (Fields_cons (field, fs))
    in
    { open_record = open_record' }

  (* Ground lens list with [unit] *)
  let sealr_with_optics :
      type record cons lens.
      (record, cons, record, lens, unit) open_record ->
      record t * lens Lens.t_list =
   fun { open_record = r } ->
    let Unwitnessed.{ name; cons; fields } = r Fields_nil in
    let rwit = Witness.make () in
    let lenses =
      let open Lens in
      let rec inner : type a l. (record, a, l, unit) fields -> l Lens.t_list =
        function
        | Fields_nil -> []
        | Fields_cons ({ fget; _ }, fs) ->
            (* TODO: build mutator from the accessors and constructor *)
            let ml = Lens.v fget (fun _ _ -> assert false) in
            ml :: inner fs
      in
      inner fields
    in
    (Record { rwit; rname = name; rfields = Fields (fields, cons) }, lenses)

  let sealr : type a b. (a, b, a, _, _) open_record -> a t =
   fun r -> sealr_with_optics r |> fst

  let ( |+ ) = app
end

module Variant (M : S.MONAD) = struct
  open Type_core.Make (M)

  open M.Infix
  module Prism = Optics.Prism (M)

  type ('v, 'pat, 'rem, 'rem_nil, 'prism, 'prism_nil) open_variant = {
    open_variant :
      'hole1 'hole2. ('v, 'rem_nil, 'hole1, 'prism_nil, 'hole2) cases ->
      string * 'hole1 * ('v, 'rem, 'hole1, 'prism, 'hole2) cases;
    next_tag : int;
  }

  let rec add_cases_to_destructor :
      type v pat pat_nil opt opt_nil.
      pat -> (v, pat, pat_nil, opt, opt_nil) cases -> pat_nil =
   fun p -> function
    | Cases_nil -> p
    | Cases_cons (C0 case, cs) -> add_cases_to_destructor (p (CV0 case)) cs
    | Cases_cons (C1 case1, cs) ->
        add_cases_to_destructor (p (fun v -> CV1 (case1, v))) cs

  let variant : string -> 'p -> ('v, 'p, 'r, 'r, 'opt, 'opt) open_variant =
   fun n p ->
    let open_variant cs = (n, add_cases_to_destructor p cs, cs) in
    { open_variant; next_tag = 0 }

  let app :
      type v c constr pat rem rem_nil opt opt_nil.
      ( v,
        pat,
        rem,
        constr -> rem_nil,
        opt,
        (v, c) Prism.mono * opt_nil )
      open_variant ->
      (v, c, constr) case ->
      (v, pat, rem, rem_nil, opt, opt_nil) open_variant =
   fun { open_variant = previous; next_tag } case ->
    let open_variant' cs = previous (Cases_cons (case next_tag, cs)) in
    { open_variant = open_variant'; next_tag = next_tag + 1 }

  let array_of_case_list cases =
    let rec inner :
        type variant pat pat_nil pri pri_nil.
        (variant, pat, pat_nil, pri, pri_nil) cases -> variant a_case list =
      function
      | Cases_nil -> []
      | Cases_cons (C0 c, cs) -> CP0 c :: inner cs
      | Cases_cons (C1 c, cs) -> CP1 c :: inner cs
    in
    inner cases |> Array.of_list

  let preview :
      type v c. (v -> v case_v M.t) -> c Witness.t -> int -> v -> c option M.t =
   fun vget type_expected tag_expected v ->
    vget v >|= function
    | CV1 ({ ctag1 = tag_actual; c1_witness = type_actual; _ }, elt) ->
        if tag_actual = tag_expected then
          Witness.cast type_actual type_expected elt
        else None
    | CV0 _ -> None

  let sealv_with_optics :
      type variant pat prisms.
      ( variant,
        pat,
        pat,
        variant -> variant case_v M.t,
        prisms,
        unit )
      open_variant ->
      variant t * prisms Prism.t_list =
   fun { open_variant = v; _ } ->
    let vname, vget, cases = v Cases_nil in
    let vwit = Witness.make () in
    let vcases = array_of_case_list cases in
    let prisms =
      let open Prism in
      let rec inner :
          type p a b. (variant, a, b, p, unit) cases -> p Prism.t_list =
        function
        | Cases_nil -> []
        | Cases_cons (C0 { c0; ctag0 = tag_expected; _ }, cs) ->
            let review () = c0 in
            let preview v =
              vget v >|= function
              | CV0 { ctag0 = tag_actual; _ } ->
                  if tag_actual = tag_expected then Some () else None
              | CV1 _ -> None
            in
            Prism.v review preview :: inner cs
        | Cases_cons
            (C1 { c1; ctag1 = tag_expected; c1_witness = type_expected; _ }, cs)
          ->
            let review = c1 in
            let preview = preview vget type_expected tag_expected in
            Prism.v review preview :: inner cs
      in
      inner cases
    in
    (Variant { vwit; vname; vcases; vget }, prisms)

  type 'a case_p = 'a case_v

  let sealv :
      type a b opt. (a, b, b, a -> a case_v M.t, opt, unit) open_variant -> a t
      =
   fun v -> sealv_with_optics v |> fst

  let ( |~ ) = app

  type ('var, 'case, 'constr) case = int -> ('var, 'case, 'constr) case_with_tag

  let case0 cname0 c0 ctag0 = C0 { ctag0; cname0; c0 }

  let case1 cname1 ctype1 c1 ctag1 =
    C1 { ctag1; cname1; ctype1; c1; c1_witness = Witness.make () }
end
