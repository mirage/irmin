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

module S = struct
  type t = int [@@deriving irmin ~equal ~short_hash]

  let hash t = short_hash t
end

module M = struct
  include Irmin.Backend.Lru.Make (S)

  let bindings t =
    let all = ref [] in
    iter t (fun k v -> all := (k, v) :: !all);
    List.sort compare !all
end

module M' = struct
  include Hashtbl.Make (S)

  let bindings t =
    let all = ref [] in
    iter (fun k v -> all := (k, v) :: !all) t;
    List.sort compare !all
end

type key = int
type action = Add of key * int | Clear

let add k v = Add (k, v)
let clear = Clear

let gen_action =
  QCheck.Gen.(frequency [ (5, map2 add small_int nat); (1, pure clear) ])

let print_action = function
  | Add (k, v) -> Fmt.str "add %d %d" k v
  | Clear -> Fmt.str "clear"

let apply t = function Add (k, v) -> M.add t k v | Clear -> M.clear t
let apply' t = function Add (k, v) -> M'.replace t k v | Clear -> M'.clear t

let run_aux create apply t =
  let state = create () in
  let rec aux = function
    | [] -> ()
    | h :: rest ->
        apply state h;
        aux rest
  in
  aux t;
  state

let run = run_aux (fun () -> M.create 100) apply
let run' = run_aux (fun () -> M'.create 100) apply'
let eq m m' = M.bindings m = M'.bindings m'
let arbitrary_action = QCheck.make gen_action ~print:print_action

let test_map =
  QCheck.Test.make ~name:"Maps" ~count:10_000
    QCheck.(list arbitrary_action)
    (fun t -> eq (run t) (run' t))

let suite = List.map QCheck_alcotest.to_alcotest [ test_map ]
