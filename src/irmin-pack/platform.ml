module type S = sig
  type path := string

  val mkdir : path -> unit

  val rename : path -> path -> unit

  val unlink : path -> unit

  val rmdir : path -> unit
end

module Unix : S = struct
  include Unix

  let mkdir p = mkdir p 0o777
end
