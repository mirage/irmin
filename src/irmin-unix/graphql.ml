module Server = struct
  module Remote = struct
    module None = struct
      let remote = None
    end
  end

  module Make
      (S : Irmin.S) (Remote : sig
          val remote : Resolver.Store.remote_fn option
      end) =
  struct
    include Irmin_graphql.Server.Make
              (Cohttp_lwt_unix.Server)
              (struct
                let info = Info.v

                let remote = Remote.remote
              end)
              (S)
  end
end
