module Server : sig
  module Remote : sig
    module None : sig
      val remote : Resolver.Store.remote_fn option
    end
  end

  module Make
      (S : Irmin.S) (Remote : sig
        val remote : Resolver.Store.remote_fn option
      end) :
    Irmin_graphql.Server.S
      with type repo = S.repo
       and type server = Cohttp_lwt_unix.Server.t

  module Make_ext
      (S : Irmin.S) (Remote : sig
        val remote : Resolver.Store.remote_fn option
      end)
      (T : Irmin_graphql.Server.CUSTOM_TYPES
             with type key := S.key
              and type metadata := S.metadata
              and type contents := S.contents
              and type hash := S.hash
              and type branch := S.branch) :
    Irmin_graphql.Server.S
      with type repo = S.repo
       and type server = Cohttp_lwt_unix.Server.t
end
