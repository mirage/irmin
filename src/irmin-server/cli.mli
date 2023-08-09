(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

val uri : Uri.t option Cmdliner.Term.t
(** Generic URI term *)

val default_uri : Uri.t
(** Default URI for command line applications *)

val config_path : string option Cmdliner.Term.t
(** Command line argument to specify configuration path *)

val codec : [ `Bin | `Json ] Cmdliner.Term.t

module Conf : sig
  module Key : sig
    val uri : Uri.t Irmin.Backend.Conf.key
  end

  val spec : Irmin.Backend.Conf.Spec.t
  val v : Irmin.config -> Uri.t option -> Irmin.config
end
