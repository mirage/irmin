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
