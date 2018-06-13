include Ir_unix

module Cli = struct
  include Ir_cli
  let add_backend = Ir_resolver.add_backend
end
