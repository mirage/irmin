type t = {
  mutable nb_freeze : int;
  mutable copied_contents : int list;
  mutable copied_nodes : int list;
  mutable copied_commits : int list;
  mutable copied_branches : int list;
}

type i = {
  t : t;
  mutable contents : int;
  mutable nodes : int;
  mutable commits : int;
  mutable branches : int;
}

let fresh_stats () =
  let t =
    {
      nb_freeze = 0;
      copied_contents = [];
      copied_nodes = [];
      copied_commits = [];
      copied_branches = [];
    }
  in
  { t; contents = 0; nodes = 0; commits = 0; branches = 0 }

let stats = fresh_stats ()

let reset_stats_i () =
  stats.contents <- 0;
  stats.nodes <- 0;
  stats.commits <- 0;
  stats.branches <- 0

let reset_stats () =
  stats.t.nb_freeze <- 0;
  stats.t.copied_contents <- [];
  stats.t.copied_nodes <- [];
  stats.t.copied_commits <- [];
  stats.t.copied_branches <- [];
  reset_stats_i ()

let get () =
  {
    nb_freeze = stats.t.nb_freeze;
    copied_contents = List.rev (stats.contents :: stats.t.copied_contents);
    copied_nodes = List.rev (stats.nodes :: stats.t.copied_nodes);
    copied_commits = List.rev (stats.commits :: stats.t.copied_commits);
    copied_branches = List.rev (stats.branches :: stats.t.copied_branches);
  }

let freeze () =
  stats.t.nb_freeze <- succ stats.t.nb_freeze;
  if stats.t.nb_freeze <> 1 then (
    stats.t.copied_contents <- stats.contents :: stats.t.copied_contents;
    stats.t.copied_nodes <- stats.nodes :: stats.t.copied_nodes;
    stats.t.copied_commits <- stats.commits :: stats.t.copied_commits;
    stats.t.copied_branches <- stats.branches :: stats.t.copied_branches;
    reset_stats_i () )

let copy_contents () = stats.contents <- succ stats.contents

let copy_nodes () = stats.nodes <- succ stats.nodes

let copy_commits () = stats.commits <- succ stats.commits

let copy_branches () = stats.branches <- succ stats.branches
