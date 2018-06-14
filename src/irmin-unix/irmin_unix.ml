include Ir_unix

module Cli = struct
  include Ir_cli
  let mk_store = Ir_resolver.mk_store
  let add_store = Ir_resolver.add_store
  let mk_contents = Ir_resolver.mk_contents
  let add_content_type = Ir_resolver.add_content_type
end
