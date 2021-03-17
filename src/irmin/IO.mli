(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** IO implementations. *)

module type S = sig
  include IO_intf.S
  (** @inline *)

  module Syntax : IO_intf.Syntax with type ('a, _) t := 'a t
end

module Lwt : S with type 'a t = 'a Lwt.t
module Direct : S with type 'a t = 'a

module type Syntax = IO_intf.Syntax
module type List = IO_intf.List

module Syntax (IO : S) : Syntax with type ('a, _) t := 'a IO.t
module List (S : S) : List with type ('a, _) t := 'a S.t

(** {2 Abstract IOs} *)

module type H = sig
  include IO_intf.H
  (** @inline *)

  module Syntax : IO_intf.Syntax with type ('a, 'io) t := ('a, 'io) t
end

module Abstract : H

(** {2 Higher-kinded IO. *)

type (+'a, 'io) t
(** The type for IO effects handling promises of type ['a] and threads of type
    ['io]. *)

module type Higher = sig
  include IO_intf.Higher

  (** {2 Converstion with abstract IOs. *)

  val run : ('a, Higher.s) Abstract.t -> 'a t
  val abstract : 'a t -> ('a, Higher.s) Abstract.t
end

module Higher (S : S) : Higher with type 'a t = 'a S.t
module Gen (S : S) : H with type ('a, _) t = 'a S.t
module Syntax' (IO : H) : Syntax with type ('a, 'io) t := ('a, 'io) IO.t
module List' (IO : H) : List with type ('a, 'io) t := ('a, 'io) IO.t
