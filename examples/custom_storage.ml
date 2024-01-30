(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

(** Create a configuration module for our storage. Here we demonstrate a simple
    configuration for setting the initial size of the hash table. *)

module Hashtbl_config = struct
  module Conf = Irmin.Backend.Conf

  let spec = Conf.Spec.v "hashtbl"
  let init_size = Conf.key ~spec "init-size" Irmin.Type.int 8
  let empty = Conf.empty spec
end

(** Create a {!Irmin.Storage.Make} functor for our hash table storage. *)

module Hashtbl_storage : Irmin.Storage.Make =
functor
  (Key : Irmin.Type.S)
  (Value : Irmin.Type.S)
  ->
  struct
    module Tbl = Hashtbl.Make (struct
      type t = Key.t

      let equal a b = Irmin.Type.(unstage (equal Key.t)) a b
      let hash k = Irmin.Type.(unstage (short_hash Key.t)) k
    end)

    (** Types *)

    type t = { t : Value.t Tbl.t; l : Eio.Mutex.t }
    type key = Key.t
    type value = Value.t

    (** Initialisation / Closing *)

    let v ~sw:_ config =
      let init_size = Irmin.Backend.Conf.get config Hashtbl_config.init_size in
      { t = Tbl.create init_size; l = Eio.Mutex.create () }

    let close _t = ()

    (** Operations *)

    let set { t; _ } key value = Tbl.replace t key value
    let mem { t; _ } key = Tbl.mem t key
    let find { t; _ } key = Tbl.find_opt t key
    let keys { t; _ } = Tbl.to_seq_keys t |> List.of_seq
    let remove { t; _ } key = Tbl.remove t key
    let clear { t; _ } = Tbl.clear t

    let batch t f =
      Eio.Mutex.lock t.l;
      let x =
        try f t
        with exn ->
          Eio.Mutex.unlock t.l;
          raise exn
      in
      Eio.Mutex.unlock t.l;
      x
  end

(** Create an Irmin store using our hash table with a specified hash type and
    content type. Irmin will create one {!Irmin.Content_addressable} store for
    storing data (keys, content, commits) and one {!Irmin.Atomic_write} store
    for storing branches. Each store will have its own hash table. *)

module Store =
  Irmin.Of_storage (Hashtbl_storage) (Irmin.Hash.SHA256) (Irmin.Contents.String)

let config ?(config = Hashtbl_config.empty) ?(init_size = 42) () =
  Irmin.Backend.Conf.add config Hashtbl_config.init_size init_size

let main () =
  Eio.Switch.run @@ fun sw ->
  let repo = Store.Repo.v ~sw (config ()) in
  let main = Store.main repo in
  let info () = Store.Info.v 0L in
  let key = "Hello" in
  Store.set_exn main [ key ] ~info "world!";
  let v = Store.get main [ key ] in
  Printf.printf "%s, %s" key v

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
