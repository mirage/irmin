(** Traces file format definitions.

    This file is also meant to be used from Tezos. OCaml version 4.09 and the
    32bit architecture should be supported.

    {3 Traces Workflow}

    A Tezos node (may) output a [Raw_replayable_trace] file. Such a trace should
    be postprocessed to create a [Replayable_trace].

    A Tezos node (may) output a [Stat_trace] file.

    {e todo0.exe} takes a [Stat_trace] file and summarises it to a
    {e boostrap_summary.json} file.

    A series of python script take a {e boostrap_summary.json} file and produce
    plots (e.g. png files).

    {e tree.exe} takes a [Replayable_trace] file, internally produces a
    [Stat_trace] file and yields it to [Todo0Module] to produce a
    {e boostrap_summary.json} file. *)

(** [Replayable_trace], a trace of Tezos's interactions with Irmin.

    {3 Interleaved Contexts and Commits}

    All the recorded operations in Tezos operate on (and create new) immutable
    records of type [context]. Most of the time, everything is linear (i.e. the
    input context to an operation is the latest output context), but there
    sometimes are several parallel chains of contexts, where all but one will
    end up being discarded.

    Similarly to contexts, commits are not always linear, i.e. a checkout may
    choose a parent that is not the latest commit.

    To solve this conundrum when replaying the trace, we need to remember all
    the [context_id -> tree] and [trace commit hash -> real commit hash] pairs
    to make sure an operation is operating on the right parent.

    In the trace, the context indices and the commit hashes are 'scoped',
    meaning that they are tagged with a boolean information indicating if this
    is the very last occurence of that value in the trace. This way we can
    discard a recorded pair as soon as possible.

    In practice, there is only 1 context and 1 commit in history, and sometimes
    0 or 2, but the code is ready for more. *)
module Replayable_trace = struct
  module V0 = struct
    let version = 0

    type header = unit [@@deriving repr]
    type 'a scope = Forget of 'a | Keep of 'a [@@deriving repr]
    type key = string list [@@deriving repr]
    type hash = string [@@deriving repr]
    type message = string [@@deriving repr]
    type context_id = int64 [@@deriving repr]

    type add = {
      key : key;
      value : string;
      in_ctx_id : context_id scope;
      out_ctx_id : context_id scope;
    }
    [@@deriving repr]

    type copy = {
      key_src : key;
      key_dst : key;
      in_ctx_id : context_id scope;
      out_ctx_id : context_id scope;
    }
    [@@deriving repr]

    type commit = {
      hash : hash scope;
      date : int64;
      message : message;
      parents : hash scope list;
      in_ctx_id : context_id scope;
    }
    [@@deriving repr]

    type row =
      (* Operation(s) that create a context from none *)
      | Checkout of hash scope * context_id scope
      (* Operations that create a context from one *)
      | Add of add
      | Remove of key * context_id scope * context_id scope
      | Copy of copy
      (* Operations that just read a context *)
      | Find of key * bool * context_id scope
      | Mem of key * bool * context_id scope
      | Mem_tree of key * bool * context_id scope
      | Commit of commit
    [@@deriving repr]
  end

  module Latest = V0
  include Latest

  include Trace_common.Io (struct
    module Latest = Latest

    (** Irmin's Replayable Bootstrap Trace *)
    let magic = Trace_common.Magic.of_string "IrmRepBT"

    let get_version_converter = function
      | 0 ->
          Trace_common.Version_converter
            {
              header_t = V0.header_t;
              row_t = V0.row_t;
              upgrade_header = Fun.id;
              upgrade_row = Fun.id;
            }
      | i -> Fmt.invalid_arg "Unknown Replayable_trace version %d" i
  end)
end
