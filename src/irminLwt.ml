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

open IrminTypes

let debug fmt = IrminMisc.debug "LWT" fmt

module Key   = IrminKey.SHA1
module Value = IrminValue.Make(Key)(IrminValue.Blob(Key))
module Tag   = IrminTag

module C = struct
  module Key = Key
  module Value = Value
  module Tag = Tag
end

module Client = IrminRemote.Client(C)
module Memory = IrminMemory.Make(C)
module Disk = IrminDisk.Make(C)
module Mix: STORE = struct
  module C = C
  module Key_store = Memory.Key_store
  module Value_store = Disk.Value_store
  module Tag_store = Disk.Tag_store
  type t = {
    memory: Memory.t;
    disk  : Disk.t;
  }
  let key_store t = Memory.key_store t.memory
  let value_store t = Disk.value_store t.disk
  let tag_store t = Disk.tag_store t.disk
end

module MemoryServer = IrminRemote.Server(Memory)
module DiskServer   = IrminRemote.Server(Disk)
module MixServer    = IrminRemote.Server(Mix)

type source =
  | Dir of string
  | Unix of string
  | InMemory

module Store = struct

  type handle =
    | Disk of Disk.t
    | Memory of Memory.t
    | Client of Client.t

  type t = {
    keys  : handle;
    values: handle;
    tags  : handle;
  }

  let create ~keys ~values ~tags =
    debug "create";
    let source s =
      match s with
      | Dir f    -> Disk (Disk.create f)
      | InMemory -> Memory (Memory.create ())
      | Unix f   ->
        let fd () =
          IrminIO.Lwt_channel.unix_socket_client f in
        Client fd in
    let keys   = source keys in
    let values = source values in
    let tags   = source tags in
    { keys; values; tags }

  type ('a,'b, 'c) s =
    | XDisk of 'a
    | XMemory of 'b
    | XClient of 'c

  let key_store t = match t.keys with
    | Disk t   -> XDisk (Disk.key_store t)
    | Memory t -> XMemory (Memory.key_store t)
    | Client t -> XClient (Client.key_store t)

  let value_store t = match t.values with
    | Disk t   -> XDisk (Disk.value_store t)
    | Memory t -> XMemory (Memory.value_store t)
    | Client t -> XClient (Client.value_store t)

  let tag_store t = match t.tags with
    | Disk t   -> XDisk (Disk.tag_store t)
    | Memory t -> XMemory (Memory.tag_store t)
    | Client t -> XClient (Client.tag_store t)

  module Key_store = struct

    module C = C

    type t = (Disk.Key_store.t, Memory.Key_store.t, Client.Key_store.t) s

    let add = function
      | XDisk t   -> Disk.Key_store.add t
      | XMemory t -> Memory.Key_store.add t
      | XClient t -> Client.Key_store.add t

    let keys = function
      | XDisk t   -> Disk.Key_store.keys t
      | XMemory t -> Memory.Key_store.keys t
      | XClient t -> Client.Key_store.keys t

    let pred = function
      | XDisk t   -> Disk.Key_store.pred t
      | XMemory t -> Memory.Key_store.pred t
      | XClient t -> Client.Key_store.pred t

  end

  module Value_store = struct

    module C = C

    type t = (Disk.Value_store.t, Memory.Value_store.t, Client.Value_store.t) s

    let read = function
      | XDisk t   -> Disk.Value_store.read t
      | XMemory t -> Memory.Value_store.read t
      | XClient t -> Client.Value_store.read t

    let write = function
      | XDisk t   -> Disk.Value_store.write t
      | XMemory t -> Memory.Value_store.write t
      | XClient t -> Client.Value_store.write t

  end

  module Tag_store = struct

    module C = C

    type t = (Disk.Tag_store.t, Memory.Tag_store.t, Client.Tag_store.t) s

    let read = function
      | XDisk t   -> Disk.Tag_store.read t
      | XMemory t -> Memory.Tag_store.read t
      | XClient t -> Client.Tag_store.read t

    let remove = function
      | XDisk t   -> Disk.Tag_store.remove t
      | XMemory t -> Memory.Tag_store.remove t
      | XClient t -> Client.Tag_store.remove t

    let all = function
      | XDisk t   -> Disk.Tag_store.all t
      | XMemory t -> Memory.Tag_store.all t
      | XClient t -> Client.Tag_store.all t

    let update = function
      | XDisk t   -> Disk.Tag_store.update t
      | XMemory t -> Memory.Tag_store.update t
      | XClient t -> Client.Tag_store.update t

  end

end

module Sync = IrminSync.Make(struct
    include Store
    module C = C
  end)

include Store
