type config := Irmin.Private.Conf.t

val lower_root : config -> string
val upper_root0 : config -> string
val upper_root1 : config -> string
val with_lower : config -> bool
val copy_in_upper : config -> bool
val blocking_copy_size : config -> int

val v :
  ?conf:config ->
  ?lower_root:string ->
  ?upper_root1:string ->
  ?upper_root0:string ->
  ?copy_in_upper:bool ->
  ?with_lower:bool ->
  ?blocking_copy_size:int ->
  unit ->
  config
