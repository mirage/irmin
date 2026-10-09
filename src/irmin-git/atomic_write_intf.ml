(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type Key = sig
  include Irmin.Branch.S

  val pp_ref : t Fmt.t
  val of_ref : string -> (t, [ `Msg of string ]) result
end

module type Sigs = sig
  module type Key = Key

  module Make (K : Key) (G : Git.S) : sig
    include Irmin.Atomic_write.S with type key = K.t and type value = G.Hash.t

    val v :
      ?lock:Eio.Mutex.t -> head:G.Reference.t option -> bare:bool -> G.t -> t
  end

  module Check_closed (S : Irmin.Atomic_write.S) : sig
    include Irmin.Atomic_write.S with type key = S.key and type value = S.value

    val v : S.t -> t
  end
end
