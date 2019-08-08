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
      (P : Irmin_graphql.Server.PRESENTATION
             with type contents := S.contents
              and type metadata := S.metadata
              and type tree := S.tree
              and type key := S.key) =
    Irmin_graphql.Server.Make_ext
      (Cohttp_lwt_unix.Server)
      (struct
        let info = Info.v

        let remote = Remote.remote
      end)
      (S)
      (P)

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
