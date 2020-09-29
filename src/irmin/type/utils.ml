open Staging

let check_valid_utf8 str =
  Uutf.String.fold_utf_8
    (fun _ _ -> function `Malformed _ -> invalid_arg "Malformed UTF-8"
      | _ -> ())
    () str

let is_valid_utf8 str =
  try
    check_valid_utf8 str;
    true
  with Invalid_argument _ -> false

let fix_staged :
    type a b. ((a -> b) staged -> (a -> b) staged) -> (a -> b) staged =
 fun unroll ->
  let rec here = lazy (unroll (stage backptr))
  and backptr e = unstage (Lazy.force here) e in
  Lazy.force here

let fix_staged2 :
    type a b c d.
    (((a -> b) staged as 'f1) -> ((c -> d) staged as 'f2) -> 'f1 * 'f2) ->
    'f1 * 'f2 =
 fun unroll ->
  let rec here = lazy (unroll (stage backptr1) (stage backptr2))
  and backptr1 e = unstage (Lazy.force here |> fst) e
  and backptr2 e = unstage (Lazy.force here |> snd) e in
  Lazy.force here
