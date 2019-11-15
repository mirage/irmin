module Server = struct
  module Remote = struct
    module None = struct
      let remote = None
    end
  end

  module Make_ext
      (S : Irmin.S) (Remote : sig
        val remote : Resolver.Store.remote_fn option
      end)
      (T : Irmin_graphql.Server.CUSTOM_TYPES
             with type key := S.key
              and type metadata := S.metadata
              and type contents := S.contents
              and type hash := S.hash
              and type branch := S.branch) =
    Irmin_graphql.Server.Make_ext
      (Cohttp_lwt_unix.Server)
      (struct
        let info = Info.v

        let remote = Remote.remote
      end)
      (S)
      (T)

  module Make
      (S : Irmin.S) (Remote : sig
        val remote : Resolver.Store.remote_fn option
      end) =
    Irmin_graphql.Server.Make
      (Cohttp_lwt_unix.Server)
      (struct
        let info = Info.v

        let remote = Remote.remote
      end)
      (S)
end
