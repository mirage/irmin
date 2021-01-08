module Default = struct
  let lower_root = Irmin_layers.Layer_id.to_string `Lower
  let upper0_root = Irmin_layers.Layer_id.to_string `Upper0
  let upper1_root = Irmin_layers.Layer_id.to_string `Upper1
  let copy_in_upper = false
  let with_lower = true
  let blocking_copy_size = 64
end

module Conf = Irmin.Private.Conf

let lower_root_key =
  Conf.key ~doc:"The root directory for the lower layer." "root_lower"
    Conf.string Default.lower_root

let lower_root conf = Conf.get conf lower_root_key

let upper_root1_key =
  Conf.key ~doc:"The root directory for the upper layer." "root_upper"
    Conf.string Default.upper1_root

let upper_root1 conf = Conf.get conf upper_root1_key

let upper_root0_key =
  Conf.key ~doc:"The root directory for the secondary upper layer."
    "root_second" Conf.string Default.upper0_root

let upper_root0 conf = Conf.get conf upper_root0_key

let copy_in_upper_key =
  Conf.key ~doc:"Copy the max commits in upper after a freeze." "copy_in_upper"
    Conf.bool false

let copy_in_upper conf = Conf.get conf copy_in_upper_key

let with_lower_key =
  Conf.key ~doc:"Use a lower layer." "with-lower" Conf.bool Default.with_lower

let with_lower conf = Conf.get conf with_lower_key

let blocking_copy_size_key =
  Conf.key
    ~doc:
      "Specify the maximum size (in bytes) that can be copied in the blocking \
       portion of the freeze."
    "blocking-copy" Conf.int Default.blocking_copy_size

let blocking_copy_size conf = Conf.get conf blocking_copy_size_key

let v ?(conf = Conf.empty) ?(lower_root = Default.lower_root)
    ?(upper_root1 = Default.upper1_root) ?(upper_root0 = Default.upper0_root)
    ?(copy_in_upper = Default.copy_in_upper) ?(with_lower = Default.with_lower)
    ?(blocking_copy_size = Default.blocking_copy_size) () =
  let with_binding k v c = Conf.add c k v in
  conf
  |> with_binding lower_root_key lower_root
  |> with_binding upper_root1_key upper_root1
  |> with_binding upper_root0_key upper_root0
  |> with_binding copy_in_upper_key copy_in_upper
  |> with_binding with_lower_key with_lower
  |> with_binding blocking_copy_size_key blocking_copy_size
