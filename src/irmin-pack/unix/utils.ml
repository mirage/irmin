(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module Object_counter : sig
  type t

  val start : unit -> t * ((unit -> unit) * (unit -> unit) * (unit -> unit))
  val finalise : t -> unit
  val finalise_with_stats : t -> int * int * int
end = struct
  type t =
    | Object_counter : {
        display : (_, _) Progress.Display.t;
        nb_commits : int ref;
        nb_nodes : int ref;
        nb_contents : int ref;
      }
        -> t

  let start () =
    let nb_commits = ref 0 in
    let nb_nodes = ref 0 in
    let nb_contents = ref 0 in
    let bar =
      let open Progress.Line in
      let count_to_string (contents, nodes, commits) =
        Fmt.str "%dk contents / %dk nodes / %d commits" (contents / 1000)
          (nodes / 1000) commits
      in
      using count_to_string string
    in
    let display = Progress.Display.start (Progress.Multi.line bar) in
    let [ reporter ] = Progress.Display.reporters display in
    let update_fn count () =
      incr count;
      reporter (!nb_contents, !nb_nodes, !nb_commits)
    in
    let contents = update_fn nb_contents
    and nodes = update_fn nb_nodes
    and commits = update_fn nb_commits in
    ( Object_counter { display; nb_contents; nb_nodes; nb_commits },
      (contents, nodes, commits) )

  let finalise (Object_counter t) = Progress.Display.finalise t.display

  let finalise_with_stats (Object_counter t as t_outer) =
    finalise t_outer;
    (!(t.nb_contents), !(t.nb_nodes), !(t.nb_commits))
end
