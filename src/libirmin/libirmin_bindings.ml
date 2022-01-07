module Stubs (I : Cstubs_inverted.INTERNAL) = struct
  include Type.Make (I)
  include Value.Make (I)
  include Info.Make (I)
  include Config.Make (I)
  include Store.Make (I)
  include Tree.Make (I)
  include Repo.Make (I)
  include Commit.Make (I)
  include Path.Make (I)
end
