module Server = struct
  module Remote = struct
    module type S = sig
      val remote: Resolver.Store.remote_fn option
    end

    module None = struct
      let remote = None
    end
  end

  module Make(S: Irmin.S)(Remote: Remote.S) = struct
    include Irmin_graphql.Server.Make(Cohttp_lwt_unix.Server)(struct
      let info = Info.v
      let remote = Remote.remote
    end)(S)
  end

  module Make_ext(S: Irmin.S)(Remote: Remote.S)(P: Irmin_graphql.Server.PRESENTATION with type Contents.t = S.contents and type Metadata.t = S.metadata) =
    struct
      include Irmin_graphql.Server.Make_ext(Cohttp_lwt_unix.Server)(struct
        let info = Info.v
        let remote = Remote.remote
      end)(S)(P)
  end
end

module Client = struct
  module Make  = Irmin_graphql.Client.Make(Cohttp_lwt_unix.Client)
end
