module Server: sig
  module Remote: sig
    module type S = sig
      val remote: Resolver.Store.remote_fn option
    end

    module None: S
  end

  module Make(S: Irmin.S)(Remote: Remote.S):
    Irmin_graphql.Server.S
      with type store = S.t
       and type server = Cohttp_lwt_unix.Server.t


  module Make_ext
    (Store : Irmin.S)
    (Remote: Remote.S)
    (Presentation : Irmin_graphql.Server.PRESENTATION with type Contents.t = Store.contents and type Metadata.t = Store.metadata):
      Irmin_graphql.Server.S with type store = Store.t
                              and type server = Cohttp_lwt_unix.Server.t
end
