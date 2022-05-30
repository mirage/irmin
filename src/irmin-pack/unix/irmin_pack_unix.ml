module Dict = Pack_dict
module Stats = Stats
module Index = Pack_index
module Inode = Inode
module Pack_store = Pack_store
module Io_legacy = Io_legacy
module Checks = Checks
module Atomic_write = Atomic_write
include Ext

module KV (Config : Irmin_pack.Conf.S) = struct
  type endpoint = unit
  type hash = Irmin.Schema.default_hash

  include Irmin_pack.Pack_key.Store_spec
  module Maker = Maker (Config)

  type metadata = Irmin.Metadata.None.t

  module Make (C : Irmin.Contents.S) = Maker.Make (Irmin.Schema.KV (C))
end
