(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type STEP = Ir_hum.S

module type S = sig
  type step
  module Step: STEP with type t = step
  include Tc.S0 with type t = Step.t list
  val to_hum: t -> string
  val of_hum: string -> t
end

module Make (S: STEP) = struct
  module Step = S
  include Tc.List(S)
  type step = S.t

  let to_hum t =
    let len = List.fold_left (fun acc s -> 1 + acc + S.size_of s) 1 t in
    let buf = Buffer.create len in
    List.iter (fun s ->
        Buffer.add_char buf '/';
        Buffer.add_string buf (S.to_hum s)
      ) t;
    Buffer.contents buf

  (* XXX: slow *)
  let of_hum s =
    List.filter ((<>)"") (Stringext.split s ~on:'/')
    |> List.map S.of_hum

end

module String = Make(struct
    include Tc.String
    let to_hum s = s
    let of_hum s = s
  end)
