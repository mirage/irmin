open! Import

module type S = sig
  type t
  type path := string

  val v : version:Version.t option -> fresh:bool -> readonly:bool -> path -> t
  val name : t -> string
  val append : t -> string -> unit
  val set : t -> off:int63 -> string -> unit
  val read : t -> off:int63 -> bytes -> int
  val read_buffer : t -> off:int63 -> buf:bytes -> len:int -> int
  val offset : t -> int63
  val force_offset : t -> int63
  val readonly : t -> bool
  val flush : t -> unit
  val close : t -> unit
  val exists : string -> bool
  val size : t -> int
  val mkdir : string -> unit

  (* {2 Versioning} *)

  val version : t -> Version.t
  val set_version : t -> Version.t -> unit
end

module type Sigs = sig
  module type S = S

  module Unix : S
end
