open! Import
module Sigs = S
module Inode = Irmin_pack.Private.Inode
module Pack = Irmin_pack.Pack

module type S = sig
  include Inode.S
  module U : Pack.S
  module L : Pack.S

  val v :
    read U.t ->
    read U.t ->
    read L.t option ->
    flip:bool ->
    freeze_in_progress:(unit -> bool) ->
    read t

  val layer_id : read t -> key -> Irmin_layers.Layer_id.t Lwt.t

  type 'a layer_type =
    | Upper : read U.t layer_type
    | Lower : read L.t layer_type

  val copy : 'l layer_type * 'l -> read t -> key -> unit
  val mem_lower : 'a t -> key -> bool Lwt.t
  val mem_next : [> read ] t -> key -> bool Lwt.t
  val next_upper : 'a t -> 'a U.t
  val current_upper : 'a t -> 'a U.t
  val lower : 'a t -> read L.t

  include S.Layered_general with type 'a t := 'a t

  val clear_caches_next_upper : 'a t -> unit

  val sync :
    ?on_generation_change:(unit -> unit) ->
    ?on_generation_change_next_upper:(unit -> unit) ->
    'a t ->
    bool

  val integrity_check :
    offset:int63 ->
    length:int ->
    layer:Irmin_layers.Layer_id.t ->
    key ->
    'a t ->
    (unit, S.integrity_error) result

  val flush : ?index:bool -> read_write t -> unit
  val copy_from_lower : dst:read_write U.t -> read t -> key -> unit Lwt.t
  val consume_newies : 'a t -> key list

  val check :
    read t ->
    ?none:(unit -> unit Lwt.t) ->
    ?some:(U.value -> unit Lwt.t) ->
    key ->
    unit Lwt.t
end

module Pack_index = Irmin_pack.Private.Pack_index

module type Sigs = sig
  module type S = S

  module Make
      (Conf : Irmin_pack.Config.S)
      (H : Irmin.Hash.S)
      (P : Sigs.Layered_pack_maker
             with type key = H.t
              and type index = Pack_index.Make(H).t)
      (Node : Irmin.Private.Node.S with type hash = H.t) :
    S
      with type key = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step
       and type index = Pack_index.Make(H).t
       and type U.index = Pack_index.Make(H).t
       and type L.index = Pack_index.Make(H).t
end
