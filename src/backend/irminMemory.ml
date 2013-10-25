(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let create () =
  Hashtbl.create 1024

module I (K: IrminKey.S) = struct

  open Lwt

  let create () =

    let table = create () in

    let module S = struct

      type key = K.t

      let init () =
        return_unit

      let write value =
        let key = K.of_buffer value in
        Hashtbl.add table key value;
        return key

      let read key =
        Printf.printf "Reading %s\n%!" (K.pretty key);
        return (
          try Some (Hashtbl.find table key)
          with Not_found -> None
        )

      let read_exn key =
        Printf.printf "Reading %s\n%!" (K.pretty key);
        try return (Hashtbl.find table key)
        with Not_found -> fail (K.Unknown key)

      let mem key =
        return (Hashtbl.mem table key)

    end
    in
    (module S: IrminStore.IRAW with type key = K.t)

end

module Tag (K: IrminKey.S) = struct

  let create () =

    let table = create () in

    let watches = create () in

    let module S = struct

      open Lwt

      include T

      type key = K.t

      exception Unknown of t

      let init () =
        return ()

      let set tag key =
        Printf.printf "Update %s to %s\n%!" (T.pretty tag) (K.pretty key);
        Hashtbl.replace table tag key;
        (* XXX: notify watches tag; *)
        return_unit

      let remove tag =
        Hashtbl.remove table tag;
        Hashtbl.remove watches tag;
        return_unit

      let read tag =
        Printf.printf "Reading %s\n%!" (T.pretty tag);
        return (
          try Some (Hashtbl.find table tag)
          with Not_found -> None
        )

      let read_exn tag =
        Printf.printf "Reading %s\n%!" (T.pretty tag);
        try return (Hashtbl.find table tag)
        with Not_found -> fail (Unknown tag)

      let mem tag =
        return (Hashtbl.mem table tag)

      let list _ =
        let elts = Hashtbl.fold (fun t _ acc -> t :: acc) table [] in
        return elts

    end in
    (module S: IrminTag.STORE with type t = T.t and type key = K.t)

end

module SimpleA = A(IrminKey.SHA1)
module SimpleM = M(IrminKey.SHA1)

module Simple = Irmin.Make
    (IrminKey.SHA1)(IrmValue.Simple)(IrminTag.Simple)
    (SimpleA)(SimpleA)(SimpleA)(SimpleM)
