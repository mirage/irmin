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
  type t = int

  let compare = ( - )
  let t = Irmin.Type.like ~compare Irmin.Type.int
end

module M = Irmin.Export_for_backends.Small_map.Make (S)
module M' = Map.Make (S)

type key = int
type action = Add of key * int | Remove of key | Update of key * int option

let add k v = Add (k, v)
let remove k = Remove k
let update k v = Update (k, v)

let gen_action =
  QCheck.Gen.(
    frequency
      [
        (1, map2 add small_int nat);
        (2, map remove small_int);
        (3, map2 update small_int (option small_int));
      ])

let print_action = function
  | Add (k, v) -> Fmt.str "Add %d %d" k v
  | Remove k -> Fmt.str "Remove %d" k
  | Update (k, v) -> Fmt.str "Update %d (%a)" k Fmt.(Dump.option int) v

let apply t = function
  | Add (k, v) -> M.add k v t
  | Remove k -> M.remove k t
  | Update (k, v) -> M.update k (fun _ -> v) t

let apply' t = function
  | Add (k, v) -> M'.add k v t
  | Remove k -> M'.remove k t
  | Update (k, v) -> M'.update k (fun _ -> v) t

let run_aux apply empty t =
  let rec aux acc = function [] -> acc | h :: t -> aux (apply acc h) t in
  aux empty t

let run = run_aux apply M.empty
let run' = run_aux apply' M'.empty
let eq m m' = M.bindings m = M'.bindings m'
let arbitrary_action = QCheck.make gen_action ~print:print_action

let test =
  QCheck.Test.make ~name:"Maps" ~count:10_000
    QCheck.(list arbitrary_action)
    (fun t -> eq (run t) (run' t))

let to_lwt_alcotest test =
  let map (x, y, f) = (x, y, fun () -> Lwt.return (f ())) in
  map (QCheck_alcotest.to_alcotest test)

let misc = [ ("small_map", [ to_lwt_alcotest test ]) ]

let () =
  Lwt_main.run
  @@ Irmin_test.Store.run "irmin-mem" ~slow:true ~misc ~sleep:Lwt_unix.sleep
       [ (`Quick, Test_mem.suite) ]
