(*
* Copyright (c) 2022 - Étienne Marais <etienne@maiste.fr>
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

let uid =
  let id = ref (-1) in
  fun () ->
    incr id;
    !id

type origin = ..

type 'a t = {
  uid : int;
  name : string;
  origin : origin option;
  repr : 'a Repr.ty;
  mutable state : 'a;
}

let state m = m.state
let set_state m v = m.state <- v

type 'a update_mode = Mutate of ('a -> unit) | Replace of ('a -> 'a)

let v :
    type a. ?origin:origin -> name:string -> initial_state:a -> a Repr.ty -> a t
    =
 fun ?origin ~name ~initial_state repr ->
  { uid = uid (); origin; name; repr; state = initial_state }

let update : type a. a t -> a update_mode -> unit =
 fun m kind ->
  match kind with Mutate f -> f m.state | Replace f -> m.state <- f m.state
