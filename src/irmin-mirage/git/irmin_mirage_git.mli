module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_mirage.endpoint

  val remote :
    ?conduit:Conduit_mirage.conduit ->
    ?resolver:Resolver_lwt.t ->
    ?headers:Cohttp.Header.t ->
    string ->
    Irmin.remote
end

module type S_MAKER = functor
  (G : Irmin_git.G)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  ->
  S
    with type key = P.t
     and type step = P.step
     and module Key = P
     and type contents = C.t
     and type branch = B.t
     and module Git = G

module type KV_MAKER = functor (G : Irmin_git.G) (C : Irmin.Contents.S) ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = string
     and module Git = G

module type REF_MAKER = functor (G : Irmin_git.G) (C : Irmin.Contents.S) ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = Irmin_git.reference
     and module Git = G

module Make : S_MAKER

module KV : KV_MAKER

module Ref : REF_MAKER

module type KV_RO = sig
  type git

  include Mirage_kv.RO

  val connect :
    ?depth:int ->
    ?branch:string ->
    ?root:key ->
    ?conduit:Conduit_mirage.t ->
    ?resolver:Resolver_lwt.t ->
    ?headers:Cohttp.Header.t ->
    git ->
    string ->
    t Lwt.t
  (** [connect ?depth ?branch ?path g uri] clones the given [uri] into [g]
      repository, using the given [branch], [depth] and ['/']-separated
      sub-[path]. By default, [branch] is master, [depth] is [1] and [path] is
      empty, ie. reads will be relative to the root of the repository. *)
end

(** Functor to create a MirageOS' KV_RO store from a Git repository. The key
    ["/HEAD"] always shows the current HEAD. *)
module KV_RO (G : Irmin_git.G) : KV_RO with type git := G.t

module type KV_RW = sig
  type git

  include Mirage_kv.RW

  val connect :
    ?depth:int ->
    ?branch:string ->
    ?root:key ->
    ?conduit:Conduit_mirage.t ->
    ?resolver:Resolver_lwt.t ->
    ?headers:Cohttp.Header.t ->
    ?author:(unit -> string) ->
    ?msg:([ `Set of key | `Remove of key | `Batch ] -> string) ->
    git ->
    string ->
    t Lwt.t
  (** [connect ?depth ?branch ?path ?author ?msg g c uri] clones the given [uri]
      into [g] repository, using the given [branch], [depth] and ['/']-separated
      sub-[path]. By default, [branch] is master, [depth] is [1] and [path] is
      empty, ie. reads will be relative to the root of the repository. [author],
      [msg] and [c] are used to create new commit info values on every update.
      By defaut [author] is [fun () -> "irmin" <irmin@mirage.io>] and [msg]
      returns basic information about the kind of operations performed. *)
end

(** Functor to create a MirageOS' KV_RW store from a Git repository. *)
module KV_RW (G : Irmin_git.G) (C : Mirage_clock.PCLOCK) :
  KV_RW with type git := G.t

(** Embed an Irmin store into an in-memory Git repository. *)
module Mem : sig
  module G : Irmin_git.G

  module Make (C : Irmin.Contents.S) (P : Irmin.Path.S) (B : Irmin.Branch.S) :
    S
      with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and module Git = G

  module Ref (C : Irmin.Contents.S) :
    S
      with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = Irmin_git.reference
       and module Git = G

  module KV (C : Irmin.Contents.S) :
    S
      with type key = Irmin.Path.String_list.t
       and type step = string
       and module Key = Irmin.Path.String_list
       and type contents = C.t
       and type branch = string
       and module Git = G

  module KV_RO : KV_RO with type git := G.t

  module KV_RW (C : Mirage_clock.PCLOCK) : KV_RW with type git := G.t
end
