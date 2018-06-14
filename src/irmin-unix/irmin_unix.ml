include Ir_unix

module Cli = struct
  include Ir_cli
  let add_store = Ir_resolver.add_store
  let add_content_type = Ir_resolver.add_content_type
end
