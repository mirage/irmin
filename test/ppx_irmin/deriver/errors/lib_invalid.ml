type t = unit [@@deriving irmin { lib = "foo" }] (* should be [Some "foo"] *)
