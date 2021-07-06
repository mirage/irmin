module type Unversioned = sig
  include Irmin.Schema.S
  module Config : Conf.S
end

module type S = sig
  include Unversioned
  module Version : Version.S
end
