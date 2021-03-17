(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open S

module type SYNC = sig
  (** {1 Native Synchronization} *)

  type +'a io
  (** The type for IO effects. *)

  type db
  (** Type type for store handles. *)

  type commit
  (** The type for store heads. *)

  type status = [ `Empty | `Head of commit ]
  (** The type for remote status. *)

  val status_t : db -> status Type.t
  (** [status_t db] is the value type for {!status} of remote [db]. *)

  val pp_status : status Fmt.t
  (** [pp_status] pretty-prints return statuses. *)

  val fetch :
    db -> ?depth:int -> remote -> (status, [ `Msg of string ]) result io
  (** [fetch t ?depth r] populate the local store [t] with objects for the
      remote store [r], using [t]'s current branch. The [depth] parameter limits
      the history depth. Return [`Empty] if either the local or remote store do
      not have a valid head. *)

  val fetch_exn : db -> ?depth:int -> remote -> status io
  (** Same as {!fetch} but raise [Invalid_argument] if either the local or
      remote store do not have a valid head. *)

  type pull_error = [ `Msg of string | Merge.conflict ]
  (** The type for pull errors. *)

  val pp_pull_error : pull_error Fmt.t
  (** [pp_push_error] pretty-prints pull errors. *)

  val pull :
    db ->
    ?depth:int ->
    remote ->
    [ `Merge of Info.f | `Set ] ->
    (status, pull_error) result io
  (** [pull t ?depth r s] is similar to {{!Sync.fetch} fetch} but it also
      updates [t]'s current branch. [s] is the update strategy:

      - [`Merge] uses [Head.merge]. Can return a conflict.
      - [`Set] uses [S.Head.set]. *)

  val pull_exn :
    db -> ?depth:int -> remote -> [ `Merge of Info.f | `Set ] -> status io
  (** Same as {!pull} but raise [Invalid_arg] in case of conflict. *)

  type push_error = [ `Msg of string | `Detached_head ]
  (** The type for push errors. *)

  val pp_push_error : push_error Fmt.t
  (** [pp_push_error] pretty-prints push errors. *)

  val push : db -> ?depth:int -> remote -> (status, push_error) result io
  (** [push t ?depth r] populates the remote store [r] with objects from the
      current store [t], using [t]'s current branch. If [b] is [t]'s current
      branch, [push] also updates the head of [b] in [r] to be the same as in
      [t].

      {b Note:} {e Git} semantics is to update [b] only if the new head if more
      recent. This is not the case in {e Irmin}. *)

  val push_exn : db -> ?depth:int -> remote -> status io
  (** Same as {!push} but raise [Invalid_argument] if an error happens. *)
end

module type REMOTE = sig
  type +'a io
  type 'a merge

  module type S =
    Store.S with type 'a io := 'a io and type 'a Merge.t = 'a merge

  type S.remote += Store : (module S with type t = 'a) * 'a -> S.remote
end

module type Sync_ext = sig
  module type SYNC = SYNC
  (** [SYNC] provides functions to synchronize an Irmin store with local and
      remote Irmin stores. *)

  module type REMOTE = REMOTE
  (** [REMOTE] provides functions to define remote stores. *)

  module Remote (IO : IO.S) : sig
    include
      REMOTE with type 'a io := 'a IO.t and type 'a merge := 'a Merge.Make(IO).t

    val remote_store : (module S with type t = 'a) -> 'a -> remote
    (** [remote_store t] is the remote corresponding to the local store [t].
        Synchronization is done by importing and exporting store {{!BC.slice}
        slices}, so this is usually much slower than native synchronization
        using {!Store.remote} but it works for all backends. *)
  end

  (** The default [Sync] implementation. *)
  module Make
      (X : Store.S)
      (R : REMOTE with type 'a io := 'a X.io and type 'a merge = 'a X.Merge.t) :
    SYNC with type db = X.t and type commit = X.commit and type 'a io := 'a X.io
end
