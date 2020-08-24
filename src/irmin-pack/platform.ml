module type S = sig
  type path := string

  val rename : path -> path -> unit

  val unlink : path -> unit
end

module Unix : S = Unix
