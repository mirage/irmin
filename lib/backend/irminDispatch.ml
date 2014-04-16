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

open Lwt

module Make
    (K: IrminKey.S)
    (C: IrminContents.S)
    (R: IrminReference.S) =
struct

  type value = (K.t, C.t) IrminValue.t

  let create ~key ~value store =

    let n = Array.length store in
    if n = 0 then failwith "Empty dispatch.";
    let (module D: Irmin.S with type Internal.key = K.t
                           and type value = C.t
                           and type Reference.key = R.t) =
      store.(0) in

    let module S = struct

      module AO = struct

        type key = D.Internal.key

        type value = D.Internal.value

        type store = Store: 't * (module Irmin.S with type Internal.key = K.t
                                                  and type value = C.t
                                                  and type Reference.key = R.t
                                                  and type Internal.t = 't) -> store

        type t = store array

        let create (): t Lwt.t =
          let l = Array.to_list store in
          Lwt_list.map_p (fun (module S: Irmin.S with type Internal.key = K.t
                                                  and type value = C.t
                                                  and type Reference.key = R.t) ->
              S.Internal.create () >>= fun t ->
              return (Store (t, (module S)))
            ) l
          >>= fun l ->
          return (Array.of_list l)

        let read t k =
          let Store (t, (module S)) = t.(key k) in
          S.Internal.read t k

        let read_exn t k =
          let Store (t, (module S)) = t.(key k) in
          S.Internal.read_exn t k

        let mem t k =
          let Store (t, (module S)) = t.(key k) in
          S.Internal.mem t k

        let list t k =
          let Store (t, (module S)) = t.(key k) in
          S.Internal.list t k

        let list t k =
          let Store (t, (module S)) = t.(key k) in
          S.Internal.list t k

        let dump t =
          let l = Array.to_list t in
          Lwt_list.map_p (function Store (t, (module S)) -> S.Internal.dump t) l
          >>= fun l ->
          return (List.concat l)

        let add t v =
          let signal, i = value v in
          let Store (t, (module S)) = t.(i) in
          S.Internal.add t v >>= fun k ->
          signal k;
          return k

      end

      module XInternal = IrminValue.Make(K)(C)(AO)
      include Irmin.Make(K)(C)(R)(XInternal)(D.Reference)

    end
    in
    (module S: Irmin.S with type Internal.key = K.t
                        and type value = C.t
                        and type Reference.key = R.t)

  let cast (module M: Irmin.S with type Internal.key = K.t
                               and type value = C.t
                               and type Reference.key = R.t) =
    (module M: Irmin.S)

end
