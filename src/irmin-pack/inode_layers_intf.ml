module Sigs = S

module type S = sig
  include Inode.S

  module U : Pack.S

  module L : Pack.S

  val v :
    [ `Read ] U.t ->
    [ `Read ] U.t ->
    [ `Read ] L.t option ->
    flip:bool ->
    freeze_lock:Lwt_mutex.t ->
    add_lock:Lwt_mutex.t ->
    [ `Read ] t

  val layer_id : [ `Read ] t -> key -> Irmin_layers.layer_id Lwt.t

  type 'a layer_type =
    | Upper : [ `Read ] U.t layer_type
    | Lower : [ `Read ] L.t layer_type

  val copy : 'l layer_type * 'l -> [ `Read ] t -> key -> unit Lwt.t

  val mem_lower : 'a t -> key -> bool Lwt.t

  val mem_next : [> `Read ] t -> key -> bool Lwt.t

  val next_upper : 'a t -> [ `Read ] U.t

  val current_upper : 'a t -> [ `Read ] U.t

  val lower : 'a t -> [ `Read ] L.t

  include S.LAYERED_GENERAL with type 'a t := 'a t

  val clear_caches_next_upper : 'a t -> unit

  val sync :
    ?on_generation_change:(unit -> unit) ->
    ?on_generation_change_next_upper:(unit -> unit) ->
    'a t ->
    bool

  val integrity_check :
    offset:int64 ->
    length:int ->
    layer:Irmin_layers.layer_id ->
    key ->
    'a t ->
    (unit, S.integrity_error) result

  val flush : ?index:bool -> 'a t -> unit

  val copy_from_lower : dst:'a U.t -> [ `Read ] t -> key -> unit Lwt.t

  val unsafe_consume_newies : 'a t -> key list

  val consume_newies : 'a t -> key list Lwt.t
end

module type Inode_layers = sig
  module type S = S

  module Make
      (Conf : Config.S)
      (H : Irmin.Hash.S)
      (P : Pack.LAYERED_MAKER
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
