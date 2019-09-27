(* Records *)
type test_record1 = {
  alpha : string;
  beta : int64 list;
  gamma : test_variant5;
}
[@@deriving irmin]

type test_record2 = {
  the_FIRST_identifier : test_record1 option;
  the_SECOND_identifier : (string, int32) result list;
}
[@@deriving irmin]
