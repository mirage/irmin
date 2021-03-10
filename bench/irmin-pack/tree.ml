open Bench_common
open Irmin.Export_for_backends

type config = {
  ncommits : int;
  ncommits_trace : int;
  depth : int;
  nchain_trees : int;
  width : int;
  nlarge_trees : int;
  root : string;
  flatten : bool;
  inode_config : [ `Entries_32 | `Entries_2 ];
  store_type : [ `Pack | `Pack_layered ];
  freeze_commit : int;
  commit_data_file : string;
  results_dir : string;
}

module type STORE = sig
  include Irmin.S with type key = string list and type contents = string

  type on_commit := int -> Hash.t -> unit Lwt.t
  type on_end := int -> unit Lwt.t
  type pp := Format.formatter -> unit

  val create_repo : config -> (Repo.t * on_commit * on_end * pp) Lwt.t
end

let pp_inode_config ppf = function
  | `Entries_2 -> Format.fprintf ppf "[2, 5]"
  | `Entries_32 -> Format.fprintf ppf "[32, 256]"

let pp_store_type ppf = function
  | `Pack -> Format.fprintf ppf "[pack store]"
  | `Pack_layered -> Format.fprintf ppf "[pack-layered store]"

let decoded_seq_of_encoded_chan_with_prefixes :
    'a Repr.ty -> in_channel -> 'a Seq.t =
 fun repr channel ->
  let decode_bin = Repr.decode_bin repr |> Repr.unstage in
  let decode_prefix = Repr.(decode_bin int32 |> unstage) in
  let produce_op () =
    try
      (* First read the prefix *)
      let prefix = really_input_string channel 4 in
      let len', len = decode_prefix prefix 0 in
      assert (len' = 4);
      let len = Int32.to_int len in
      (* Then read the repr *)
      let content = really_input_string channel len in
      let len', op = decode_bin content 0 in
      assert (len' = len);
      Some (op, ())
    with End_of_file -> None
  in
  Seq.unfold produce_op ()

let seq_mapi64 s =
  let i = ref Int64.minus_one in
  Seq.map
    (fun v ->
      i := Int64.succ !i;
      (!i, v))
    s

module Exponential_moving_average = struct
  type t = {
    momentum : float;
    opp_momentum : float;
    value : float;
    count : float;
  }

  (** [create m] is [ema], a functional exponential moving average. [1. -. m] is
      the fraction of what's forgotten of the past during each [update].

      When [m = 0.], all the past is forgotten on each [update], i.e. [peek ema]
      is the latest point fed to [update]. *)
  let create momentum =
    if momentum < 0. || momentum >= 1. then invalid_arg "Wrong momentum";
    { momentum; opp_momentum = 1. -. momentum; value = 0.; count = 0. }

  (** [from_half_life hl] is [ema], a functional exponential moving average.
      After [hl] calls to [update], half of the past is forgotten. *)
  let from_half_life hl =
    if hl < 0. then invalid_arg "Wrong half life";
    (if hl = 0. then 0. else log 0.5 /. hl |> exp) |> create

  (** [from_half_life_ratio hl_ratio step_count] is [ema], a functional
      exponential moving average. After [hl_ratio * step_count] calls to
      [update], half of the past is forgotten. *)
  let from_half_life_ratio hl_ratio step_count =
    if hl_ratio < 0. then invalid_arg "Wrong half life ratio";
    if step_count <= 0L then invalid_arg "Wront step count";
    Int64.to_float step_count *. hl_ratio |> from_half_life

  (** Feed a new point to the EMA. *)
  let update ema point =
    let value = (ema.value *. ema.momentum) +. (point *. ema.opp_momentum) in
    let count = ema.count +. 1. in
    { ema with value; count }

  (** Read the EMA value. *)
  let peek ema =
    if ema.count = 0. then failwith "Can't peek an EMA before first update";
    ema.value /. (1. -. (ema.momentum ** ema.count))
end

module Bootstrap_trace = struct
  type 'a scope = Forget of 'a | Keep of 'a [@@deriving repr]
  type key = string list [@@deriving repr]
  type hash = string [@@deriving repr]
  type message = string [@@deriving repr]

  type add = {
    key : key;
    value : string;
    ctx0 : int64 scope;
    ctx1 : int64 scope;
  }
  [@@deriving repr]

  type copy = {
    key_src : key;
    key_dst : key;
    ctx0 : int64 scope;
    ctx1 : int64 scope;
  }
  [@@deriving repr]

  type commit = {
    hash : hash scope;
    date : int64;
    message : message;
    parents : hash scope list;
    ctx0 : int64 scope;
  }
  [@@deriving repr]

  (** The 8 different operations recorded in Tezos.

      {3 Interleaved Contexts and Commits}

      All the recorded operations in Tezos operate on (and create new) immutable
      records of type [context]. Most of the time, everything is linear (i.e.
      the input context to an operation is the latest output context), but there
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

      In practice, there is only 1 context and 1 commit in history, and
      sometimes 0 or 2, but the code is ready for more. *)
  type op =
    (* Operation(s) that create a context from none *)
    | Checkout of hash scope * int64 scope
    (* Operations that create a context from one *)
    | Add of add
    | Remove of key * int64 scope * int64 scope
    | Copy of copy
    (* Operations that just read a context *)
    | Find of key * bool * int64 scope
    | Mem of key * bool * int64 scope
    | Mem_tree of key * bool * int64 scope
    | Commit of commit
  [@@deriving repr]

  let is_hex_char = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false

  let is_2char_hex s =
    if String.length s <> 2 then false
    else s |> String.to_seq |> List.of_seq |> List.for_all is_hex_char

  let is_30char_hex s =
    if String.length s <> 30 then false
    else s |> String.to_seq |> List.of_seq |> List.for_all is_hex_char

  let rec flatten_key_suffix = function
    | a :: b :: c :: d :: e :: f :: tl
      when is_2char_hex a
           && is_2char_hex b
           && is_2char_hex c
           && is_2char_hex d
           && is_2char_hex e
           && is_30char_hex f ->
        (a ^ b ^ c ^ d ^ e ^ f) :: flatten_key_suffix tl
    | hd :: tl -> hd :: flatten_key_suffix tl
    | [] -> []

  (** This function flattens all the 6 step-long chunks forming 40 byte-long
      hashes to a single step. Those flattenings are performed during the trace
      replay, i.e. they count in the total time.

      The paths in tezos:
      https://www.dailambda.jp/blog/2020-05-11-plebeia/#tezos-path

      A chopped hash has this form:

      {v ([0-9a-f]{2}/){5}[0-9a-f]{30} v}

      and is flattened to that form:

      {v [0-9a-f]{40} v} *)
  let flatten_key = flatten_key_suffix

  let flatten_op = function
    | Checkout _ as op -> op
    | Add op -> Add { op with key = flatten_key op.key }
    | Remove (keys, ctx0, ctx1) -> Remove (flatten_key keys, ctx0, ctx1)
    | Copy op ->
        Copy
          {
            op with
            key_src = flatten_key op.key_src;
            key_dst = flatten_key op.key_dst;
          }
    | Find (keys, b, ctx) -> Find (flatten_key keys, b, ctx)
    | Mem (keys, b, ctx) -> Mem (flatten_key keys, b, ctx)
    | Mem_tree (keys, b, ctx) -> Mem_tree (flatten_key keys, b, ctx)
    | Commit _ as op -> op

  let open_ops_sequence path : op Seq.t =
    let chan = open_in_bin path in
    decoded_seq_of_encoded_chan_with_prefixes op_t chan

  let open_commit_sequence max_ncommits flatten path : op list Seq.t =
    let rec aux (ops_seq, commits_sent, ops) =
      if commits_sent >= max_ncommits then None
      else
        match ops_seq () with
        | Seq.Nil -> None
        | Cons ((Commit _ as op), ops_seq) ->
            let ops = op :: ops |> List.rev in
            Some (ops, (ops_seq, commits_sent + 1, []))
        | Cons (op, ops_seq) ->
            let op = if flatten then flatten_op op else op in
            aux (ops_seq, commits_sent, op :: ops)
    in
    let ops_seq = open_ops_sequence path in
    Seq.unfold aux (ops_seq, 0, [])

  (** Stats derived from ops durations.

      First, the stats are saved to disk while replaying the trace, using
      [Stats.push_duration]. Then, a summary is computed using
      [Stats.Summary.summarise]. Finally, the disk has to be cleaned using
      [Stats.cleanup]. *)
  module Stats = struct
    type stat_entry =
      [ `Add | `Remove | `Find | `Mem | `Mem_tree | `Checkout | `Copy | `Commit ]
    [@@deriving repr]

    let op_tags =
      [ `Add; `Remove; `Find; `Mem; `Mem_tree; `Checkout; `Copy; `Commit ]

    type per_op = {
      path : string;
      channel : out_channel;
      mutable count : int64;
    }

    type t = stat_entry -> per_op

    let create_op cache_dir op =
      let path =
        Repr.to_string stat_entry_t op
        |> String.to_seq
        |> Seq.filter (function '"' -> false | _ -> true)
        |> String.of_seq
        |> String.lowercase_ascii
        |> Printf.sprintf "%s_durations"
        |> Filename.concat cache_dir
      in
      let channel = open_out path in
      { path; channel; count = 0L }

    let create cache_dir =
      let tbl =
        op_tags
        |> List.map (fun op -> (op, create_op cache_dir op))
        |> List.to_seq
        |> Hashtbl.of_seq
      in
      Hashtbl.find tbl

    let cleanup t = List.iter (fun op -> Sys.remove (t op).path) op_tags

    let write_duration : out_channel -> float -> unit =
      let encode_duration = Repr.(encode_bin int32 |> unstage) in
      fun channel d ->
        encode_duration (Int32.bits_of_float d) (output_string channel)

    let push_duration t op d : unit =
      let t = t op in
      t.count <- Int64.add t.count 1L;
      write_duration t.channel d

    let read_duration : in_channel -> float =
      let decode_duration = Repr.(decode_bin int32 |> unstage) in
      fun channel ->
        let s = really_input_string channel 4 in
        decode_duration s 0 |> snd |> Int32.float_of_bits

    let open_duration_sequence path count : float Seq.t =
      let channel = open_in path in
      assert (LargeFile.in_channel_length channel = Int64.mul count 4L);
      let aux i =
        if i >= count then None else Some (read_duration channel, Int64.add i 1L)
      in
      Seq.unfold aux 0L

    (** The computed summaries contain 3 informations for each operations.

        {3 Histograms}

        The [histo] field is computed using https://github.com/barko/bentov.

        [Bentov] computes dynamic histograms without the need for a priori
        information on the distributions, while maintaining a constant memory
        space and a marginal CPU footprint.

        The implementation of that library is pretty straightforward, but not
        perfect; it doesn't scale well with the number of bins.

        The computed histogram depends on the order of the operations, some
        marginal unsabilities are to be expected.

        [Bentov] is good at spreading the bins on the input space. Since these
        data will be shown on a log plot, the log10 of those values is passed to
        [Bentov] instead, but the json will store real seconds.

        {3 Moving Averages}

        [ma_xs] and [ma_ys] form a smoothed curve describing the variations over
        time of the call durations to an op.

        [ma_xs] is an increasing list of float. The last element is [1.0] and
        the first is [1 / moving_average_sample_count].

        [ma_ys] is a list of call durations smoothed using an exponential decay
        moving average, defined from its half life through the
        [moving_average_half_life_ratio] constant.

        {3 Max Values}

        The [max_point] field contains the value and the index of the longest
        occurence of an operation. *)
    module Summary = struct
      let histo_bin_count = 16
      let moving_average_sample_count = 200
      let moving_average_half_life_ratio = 1. /. 40.

      type accumulator = {
        histo : Bentov.histogram;
        ma : Exponential_moving_average.t;
        ma_points : float list;
        next_ma_points : int64 list;
        max_point : int64 * float;
      }

      type per_op = {
        histo : Bentov.histogram;
        ma_xs : float list;
        ma_ys : float list;
        max_point : int64 * float;
      }

      type t = stat_entry -> per_op

      let linear_histo_of_log10_histo histo =
        let open Bentov in
        List.fold_left
          (fun histo { center; count } ->
            let center = Float.pow 10. center in
            addc center count histo)
          (create histo_bin_count) (bins histo)

      let summary_of_accumulator v acc0 acc =
        assert (List.length acc.ma_points = List.length acc0.next_ma_points);
        assert (List.length acc.next_ma_points = 0);
        let histo = linear_histo_of_log10_histo acc.histo in
        let ma_xs =
          if v.count = 1L then [ 0.5 ]
          else
            List.map
              (fun x -> Int64.to_float x /. (Int64.to_float v.count -. 1.))
              acc0.next_ma_points
        in
        let ma_ys = List.rev acc.ma_points in
        { histo; ma_xs; ma_ys; max_point = acc.max_point }

      (** Iterate over the durations stored on disk, while accumulating an
          [accumulator]. Finish by turning it to a [per_op]. *)
      let summarise_op v : per_op =
        flush v.channel;
        let aux (acc : accumulator) (i, duration) =
          let histo = Bentov.add (Float.log10 duration) acc.histo in
          let ma = Exponential_moving_average.update acc.ma duration in
          let ma_points, next_ma_points =
            if List.hd acc.next_ma_points <> i then
              (acc.ma_points, acc.next_ma_points)
            else
              ( Exponential_moving_average.peek ma :: acc.ma_points,
                List.tl acc.next_ma_points )
          in
          let max_point =
            if snd acc.max_point >= duration then acc.max_point
            else (i, duration)
          in
          { histo; ma; ma_points; next_ma_points; max_point }
        in
        let acc0 =
          {
            histo = Bentov.create histo_bin_count;
            ma =
              Exponential_moving_average.from_half_life_ratio
                moving_average_half_life_ratio v.count;
            ma_points = [];
            next_ma_points =
              List.init moving_average_sample_count (fun i ->
                  float_of_int (i + 1)
                  /. float_of_int moving_average_sample_count
                  *. (Int64.to_float v.count -. 1.)
                  |> Float.floor
                  |> Int64.of_float)
              (* Dedup in case [v.count] is very small *)
              |> List.sort_uniq compare;
            max_point = (0L, -.Float.infinity);
          }
        in
        open_duration_sequence v.path v.count
        |> seq_mapi64
        |> Seq.fold_left aux acc0
        |> summary_of_accumulator v acc0

      let summarise_op v : per_op =
        if v.count = 0L then
          {
            histo = Bentov.create histo_bin_count;
            ma_xs = [];
            ma_ys = [];
            max_point = (0L, 0.);
          }
        else summarise_op v

      let summarise stats : t =
        let tbl =
          op_tags
          |> List.map (fun op -> (op, stats op |> summarise_op))
          |> List.to_seq
          |> Hashtbl.of_seq
        in
        Hashtbl.find tbl
    end
  end
end

module Generate_trees_from_trace (Store : STORE) = struct
  type context = { tree : Store.tree }

  type t = {
    contexts : (int64, context) Hashtbl.t;
    hash_corresps : (Bootstrap_trace.hash, Store.Hash.t) Hashtbl.t;
    mutable latest_commit : Store.Hash.t option;
  }

  let pp_stats ppf
      (summary, as_json, flatten, inode_config, store_type, elapsed_cpu, elapsed)
      =
    let stat_entry_t = Bootstrap_trace.Stats.stat_entry_t in
    let op_tags = Bootstrap_trace.Stats.op_tags in
    let mean histo =
      if Bentov.total_count histo > 0 then Bentov.mean histo else 0.
    in
    let total =
      op_tags
      |> List.to_seq
      |> Seq.map (fun op -> (summary op).Bootstrap_trace.Stats.Summary.histo)
      |> Seq.map (fun histo ->
             mean histo *. float_of_int (Bentov.total_count histo))
      |> Seq.fold_left ( +. ) 0.
    in
    let total = if total = 0. then 1. else total in
    let pp_max ppf which =
      let max_idx, max_duration =
        (summary which).Bootstrap_trace.Stats.Summary.max_point
      in
      if as_json then
        Format.fprintf ppf "%a:[%Ld, %f]" (Repr.pp stat_entry_t) which max_idx
          max_duration
      else
        Format.fprintf ppf "%a with id %Ld lasted %.6f sec"
          (Repr.pp stat_entry_t) which max_idx max_duration
    in
    let pp_moving_average ppf which =
      let xs = (summary which).Bootstrap_trace.Stats.Summary.ma_xs in
      let ys = (summary which).Bootstrap_trace.Stats.Summary.ma_ys in
      Format.fprintf ppf "  %a:{\"xs\": [%a], \"ys\": [%a]}"
        (Repr.pp stat_entry_t) which
        Fmt.(list ~sep:(any ",") float)
        xs
        Fmt.(list ~sep:(any ",") float)
        ys
    in
    let pp_histo ppf which =
      let histo = (summary which).Bootstrap_trace.Stats.Summary.histo in
      let n = Bentov.total_count histo in
      let el = mean histo *. float_of_int n in
      if as_json then
        let pp_bar ppf (bin : Bentov.bin) =
          Format.fprintf ppf "[%2d,%.3e]" bin.count bin.center
        in
        Format.fprintf ppf "  %a:[%a]" (Repr.pp stat_entry_t) which
          Fmt.(list ~sep:(any ",") pp_bar)
          (Bentov.bins histo)
      else
        Format.fprintf ppf
          "%a was called %d times for a total of %.3f sec (%.1f%%)"
          (Repr.pp stat_entry_t) which n el
          (el /. total *. 100.)
    in
    if as_json then
      Fmt.pf ppf
        "{\"revision\":\"%s\", \"flatten\":%d, \"inode_config\":\"%a\", \
         \"store_type\":\"%a\", \"elapsed_cpu\":\"%f\", \"elapsed\":\"%f\",@\n\
         \"max_durations\":{\n\
         %a},\n\
         \"moving_average_points\":{\n\
         %a},\n\
         \"histo_points\":{\n\
         %a}}"
        "missing"
        (if flatten then 1 else 0)
        pp_inode_config inode_config pp_store_type store_type elapsed_cpu
        elapsed
        Fmt.(list ~sep:(any ",@\n") pp_max)
        op_tags
        Fmt.(list ~sep:(any ",@\n") pp_moving_average)
        op_tags
        Fmt.(list ~sep:(any ",@\n") pp_histo)
        op_tags
    else
      Fmt.pf ppf "%a\n%a"
        Fmt.(list ~sep:(any "@\n") pp_histo)
        op_tags
        Fmt.(list ~sep:(any "@\n") pp_max)
        op_tags

  let with_monitoring stats which f =
    let t0 = Mtime_clock.counter () in
    let+ res = f () in
    Mtime_clock.count t0
    |> Mtime.Span.to_s
    |> Bootstrap_trace.Stats.push_duration stats which;
    res

  let error_find op k b n_op n_c ctx0 =
    Fmt.failwith
      "Cannot reproduce operation %d on ctx %Ld of commit %d %s @[k = %a@] \
       expected %b"
      n_op ctx0 n_c op
      Fmt.(list ~sep:comma string)
      k b

  let unscope = function Bootstrap_trace.Forget v -> v | Keep v -> v

  let maybe_forget_hash t = function
    | Bootstrap_trace.Forget h -> Hashtbl.remove t.hash_corresps h
    | Keep _ -> ()

  let maybe_forget_ctx t = function
    | Bootstrap_trace.Forget ctx -> Hashtbl.remove t.contexts ctx
    | Keep _ -> ()

  let exec_checkout t repo h_trace ctx1 () =
    let h_store = Hashtbl.find t.hash_corresps (unscope h_trace) in
    maybe_forget_hash t h_trace;
    Store.Commit.of_hash repo h_store >|= function
    | None -> failwith "prev commit not found"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        Hashtbl.add t.contexts (unscope ctx1) { tree };
        maybe_forget_ctx t ctx1

  let exec_add t key v ctx0 ctx1 () =
    let { tree } = Hashtbl.find t.contexts (unscope ctx0) in
    maybe_forget_ctx t ctx0;
    let+ tree = Store.Tree.add tree key v in
    Hashtbl.add t.contexts (unscope ctx1) { tree };
    maybe_forget_ctx t ctx1

  let exec_remove t keys ctx0 ctx1 () =
    let { tree } = Hashtbl.find t.contexts (unscope ctx0) in
    maybe_forget_ctx t ctx0;
    let+ tree = Store.Tree.remove tree keys in
    Hashtbl.add t.contexts (unscope ctx1) { tree };
    maybe_forget_ctx t ctx1

  let exec_copy t from to_ ctx0 ctx1 () =
    let { tree } = Hashtbl.find t.contexts (unscope ctx0) in
    maybe_forget_ctx t ctx0;
    Store.Tree.find_tree tree from >>= function
    | None -> failwith "Couldn't find tree in exec_copy"
    | Some sub_tree ->
        let* tree = Store.Tree.add_tree tree to_ sub_tree in
        Hashtbl.add t.contexts (unscope ctx1) { tree };
        maybe_forget_ctx t ctx1;
        Lwt.return_unit

  let exec_find t n i keys b ctx0 () =
    let { tree } = Hashtbl.find t.contexts (unscope ctx0) in
    maybe_forget_ctx t ctx0;
    Store.Tree.find tree keys >|= function
    | None when not b -> ()
    | Some _ when b -> ()
    | _ -> error_find "find" keys b i n (unscope ctx0)

  let exec_mem t n i keys b ctx0 () =
    let { tree } = Hashtbl.find t.contexts (unscope ctx0) in
    maybe_forget_ctx t ctx0;
    let+ b' = Store.Tree.mem tree keys in
    if b <> b' then error_find "mem" keys b i n (unscope ctx0)

  let exec_mem_tree t n i keys b ctx0 () =
    let { tree } = Hashtbl.find t.contexts (unscope ctx0) in
    maybe_forget_ctx t ctx0;
    let+ b' = Store.Tree.mem_tree tree keys in
    if b <> b' then error_find "mem_tree" keys b i n (unscope ctx0)

  let exec_commit t repo h_trace date message parents_trace ctx0 () =
    let parents_store =
      parents_trace
      |> List.map unscope
      |> List.map (Hashtbl.find t.hash_corresps)
    in
    List.iter (maybe_forget_hash t) parents_trace;
    let { tree } = Hashtbl.find t.contexts (unscope ctx0) in
    maybe_forget_ctx t ctx0;
    let* _ =
      (* in tezos commits call Tree.list first for the unshallow operation *)
      Store.Tree.list tree []
    in
    let info = Irmin.Info.v ~date ~author:"Tezos" message in
    let+ commit = Store.Commit.v repo ~info ~parents:parents_store tree in
    Store.Tree.clear tree;
    let h_store = Store.Commit.hash commit in
    (* It's okey to have [h_trace] already in history. It corresponds to
     * re-commiting the same thing, hence the [.replace] below. *)
    Hashtbl.replace t.hash_corresps (unscope h_trace) h_store;
    maybe_forget_hash t h_trace;
    t.latest_commit <- Some h_store

  let add_operations t repo operations n stats =
    let rec aux l i =
      match l with
      | Bootstrap_trace.Checkout (h, ctx1) :: tl ->
          exec_checkout t repo h ctx1 |> with_monitoring stats `Checkout
          >>= fun () -> aux tl (i + 1)
      | Add op :: tl ->
          exec_add t op.key op.value op.ctx0 op.ctx1
          |> with_monitoring stats `Add
          >>= fun () -> aux tl (i + 1)
      | Remove (keys, ctx0, ctx1) :: tl ->
          exec_remove t keys ctx0 ctx1 |> with_monitoring stats `Remove
          >>= fun () -> aux tl (i + 1)
      | Copy op :: tl ->
          exec_copy t op.key_src op.key_dst op.ctx0 op.ctx1
          |> with_monitoring stats `Copy
          >>= fun () -> aux tl (i + 1)
      | Find (keys, b, ctx0) :: tl ->
          exec_find t n i keys b ctx0 |> with_monitoring stats `Find
          >>= fun () -> aux tl (i + 1)
      | Mem (keys, b, ctx0) :: tl ->
          exec_mem t n i keys b ctx0 |> with_monitoring stats `Mem >>= fun () ->
          aux tl (i + 1)
      | Mem_tree (keys, b, ctx0) :: tl ->
          exec_mem_tree t n i keys b ctx0 |> with_monitoring stats `Mem_tree
          >>= fun () -> aux tl (i + 1)
      | [ Commit op ] ->
          exec_commit t repo op.hash op.date op.message op.parents op.ctx0
          |> with_monitoring stats `Commit
      | Commit _ :: _ | [] ->
          failwith "A batch of operation should end with a commit"
    in
    aux operations 0

  let add_commits repo max_ncommits commit_seq on_commit on_end stats () =
    with_progress_bar ~message:"Replaying trace" ~n:max_ncommits ~unit:"commits"
    @@ fun prog ->
    let t =
      {
        contexts = Hashtbl.create 3;
        hash_corresps = Hashtbl.create 3;
        latest_commit = None;
      }
    in

    (* Manually add genesis context *)
    Hashtbl.add t.contexts 0L { tree = Store.Tree.empty };

    let rec aux commit_seq i =
      match commit_seq () with
      | Seq.Nil ->
          (* Let's print the length of [data/commitments] using t.latest_commit
             some day. Today it requires loading everything. *)
          on_end i >|= fun () -> i
      | Cons (ops, commit_seq) ->
          let* () = add_operations t repo ops i stats in

          let len0 = Hashtbl.length t.contexts in
          let len1 = Hashtbl.length t.hash_corresps in
          if (len0, len1) <> (0, 1) then
            Printf.eprintf "\n> After commit %6d we have %d/%d history size\n%!"
              i len0 len1;
          let* () = on_commit i (Option.get t.latest_commit) in
          prog Int64.one;
          aux commit_seq (i + 1)
    in
    aux commit_seq 0
end

module Benchmark = struct
  type result = { time : float; size : int }

  let run config f =
    let+ time, res = with_timer f in
    let size = FSHelper.get_size config.root in
    ({ time; size }, res)

  let pp_results ppf result =
    Format.fprintf ppf "Total time: %f@\nSize on disk: %d M" result.time
      result.size
end

module Hash = Irmin.Hash.SHA1

module Bench_suite (Store : STORE) = struct
  let init_commit repo =
    Store.Commit.v repo ~info:(info ()) ~parents:[] Store.Tree.empty

  module Trees = Generate_trees (Store)
  module Trees_trace = Generate_trees_from_trace (Store)

  let checkout_and_commit repo prev_commit f =
    Store.Commit.of_hash repo prev_commit >>= function
    | None -> Lwt.fail_with "commit not found"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let* tree = f tree in
        Store.Commit.v repo ~info:(info ()) ~parents:[ prev_commit ] tree

  let add_commits ~message repo ncommits on_commit on_end f () =
    with_progress_bar ~message ~n:ncommits ~unit:"commits" @@ fun prog ->
    let* c = init_commit repo in
    let rec aux c i =
      if i >= ncommits then on_end i
      else
        let* c' = checkout_and_commit repo (Store.Commit.hash c) f in
        let* () = on_commit i (Store.Commit.hash c') in
        prog Int64.one;
        aux c' (i + 1)
    in
    aux c 0

  let run_large config =
    reset_stats ();
    let* repo, on_commit, on_end, repo_pp = Store.create_repo config in
    let* result, () =
      Trees.add_large_trees config.width config.nlarge_trees
      |> add_commits ~message:"Playing large mode" repo config.ncommits
           on_commit on_end
      |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in
    fun ppf ->
      Format.fprintf ppf
        "Large trees mode on inode config %a, %a: %d commits, each consisting \
         of %d large trees of %d entries\n\
         %t@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        config.ncommits config.nlarge_trees config.width repo_pp
        Benchmark.pp_results result

  let run_chains config =
    reset_stats ();
    let* repo, on_commit, on_end, repo_pp = Store.create_repo config in
    let* result, () =
      Trees.add_chain_trees config.depth config.nchain_trees
      |> add_commits ~message:"Playing chain mode" repo config.ncommits
           on_commit on_end
      |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in
    fun ppf ->
      Format.fprintf ppf
        "Chain trees mode on inode config %a, %a: %d commits, each consisting \
         of %d chains of depth %d\n\
         %t@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        config.ncommits config.nchain_trees config.depth repo_pp
        Benchmark.pp_results result

  let run_read_trace config stats =
    let commit_seq =
      Bootstrap_trace.open_commit_sequence config.ncommits_trace config.flatten
        config.commit_data_file
    in
    let* repo, on_commit, on_end, repo_pp = Store.create_repo config in

    let t0_cpu = Sys.time () in
    let t0 = Mtime_clock.counter () in
    let* result, n =
      Trees_trace.add_commits repo config.ncommits_trace commit_seq on_commit
        on_end stats
      |> Benchmark.run config
    in
    let elapsed_cpu = Sys.time () -. t0_cpu in
    let elapsed = Mtime_clock.count t0 |> Mtime.Span.to_s in

    let+ () = Store.Repo.close repo in

    let config = { config with ncommits_trace = n } in
    let stats = Bootstrap_trace.Stats.Summary.summarise stats in

    let json_path =
      let ( / ) = Filename.concat in
      config.results_dir / "boostrap_trace_timings.json"
    in
    let json_channel = open_out json_path in
    Format.fprintf
      (Format.formatter_of_out_channel json_channel)
      "%a%!" Trees_trace.pp_stats
      ( stats,
        true,
        config.flatten,
        config.inode_config,
        config.store_type,
        elapsed_cpu,
        elapsed );
    close_out json_channel;

    fun ppf ->
      Format.fprintf ppf
        "Tezos_log mode on inode config %a, %a. @\n\
         %t@\n\
         Results: @\n\
         %a@\n\
         Stats saved to %s@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        repo_pp Trees_trace.pp_stats
        ( stats,
          false,
          config.flatten,
          config.inode_config,
          config.store_type,
          elapsed_cpu,
          elapsed )
        json_path Benchmark.pp_results result

  let run_read_trace config =
    reset_stats ();
    prepare_results_dir config.results_dir;
    let stats = Bootstrap_trace.Stats.create config.results_dir in
    try
      let res = run_read_trace config stats in
      Bootstrap_trace.Stats.cleanup stats;
      res
    with e ->
      Bootstrap_trace.Stats.cleanup stats;
      raise e
end

module Make_store_layered (Conf : sig
  val entries : int
  val stable_hash : int
end) =
struct
  module Store =
    Irmin_pack_layered.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Hash)

  let create_repo config =
    let conf = Irmin_pack.config ~readonly:false ~fresh:true config.root in
    let* repo = Store.Repo.v conf in
    let on_commit i commit_hash =
      let* () =
        if i = config.freeze_commit then
          let* c = Store.Commit.of_hash repo commit_hash in
          let c = Option.get c in
          Store.freeze repo ~max:[ c ] ~min_upper:[ c ]
        else Lwt.return_unit
      in
      (* Something else than pause could be used here, like an Lwt_unix.sleep
         or nothing. See #1293 *)
      Lwt.pause ()
    in
    let on_end _i = Store.PrivateLayer.wait_for_freeze repo in
    let pp ppf =
      if Irmin_layers.Stats.get_freeze_count () = 0 then
        Format.fprintf ppf "no freeze"
      else Format.fprintf ppf "%t" Irmin_layers.Stats.pp_latest
    in
    Lwt.return (repo, on_commit, on_end, pp)

  include Store
end

module Make_store_pack (Conf : sig
  val entries : int
  val stable_hash : int
end) =
struct
  module Store =
    Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Hash)

  let create_repo config =
    let conf = Irmin_pack.config ~readonly:false ~fresh:true config.root in
    let* repo = Store.Repo.v conf in
    let on_commit _ _ = Lwt.return_unit in
    let on_end _ = Lwt.return_unit in
    let pp _ = () in
    Lwt.return (repo, on_commit, on_end, pp)

  include Store
end

module Conf2 = struct
  let entries = 2
  let stable_hash = 5
end

module Bench_inodes_32 = Bench_suite (Make_store_pack (Conf))
module Bench_inodes_2 = Bench_suite (Make_store_pack (Conf2))
module Bench_inodes_32_layered = Bench_suite (Make_store_layered (Conf))
module Bench_inodes_2_layered = Bench_suite (Make_store_layered (Conf2))

type suite_elt = {
  mode : [ `Read_trace | `Chains | `Large ];
  speed : [ `Quick | `Slow | `Custom ];
  run : config -> (Format.formatter -> unit) Lwt.t;
}

let suite : suite_elt list =
  [
    {
      mode = `Read_trace;
      speed = `Quick;
      run =
        (fun config ->
          Bench_inodes_32.run_read_trace
            { config with inode_config = `Entries_32; store_type = `Pack });
    };
    {
      mode = `Read_trace;
      speed = `Slow;
      run =
        (fun config ->
          Bench_inodes_32.run_read_trace
            { config with inode_config = `Entries_32; store_type = `Pack });
    };
    {
      mode = `Chains;
      speed = `Quick;
      run =
        (fun config ->
          Bench_inodes_32.run_chains
            { config with inode_config = `Entries_32; store_type = `Pack });
    };
    {
      mode = `Chains;
      speed = `Slow;
      run =
        (fun config ->
          Bench_inodes_2.run_chains
            { config with inode_config = `Entries_2; store_type = `Pack });
    };
    {
      mode = `Large;
      speed = `Quick;
      run =
        (fun config ->
          Bench_inodes_32.run_large
            { config with inode_config = `Entries_32; store_type = `Pack });
    };
    {
      mode = `Large;
      speed = `Slow;
      run =
        (fun config ->
          Bench_inodes_2.run_large
            { config with inode_config = `Entries_2; store_type = `Pack });
    };
    {
      mode = `Read_trace;
      speed = `Custom;
      run =
        (fun config ->
          match (config.inode_config, config.store_type) with
          | `Entries_2, `Pack -> Bench_inodes_2.run_read_trace config
          | `Entries_32, `Pack -> Bench_inodes_32.run_read_trace config
          | `Entries_2, `Pack_layered ->
              Bench_inodes_2_layered.run_read_trace config
          | `Entries_32, `Pack_layered ->
              Bench_inodes_32_layered.run_read_trace config);
    };
    {
      mode = `Chains;
      speed = `Custom;
      run =
        (fun config ->
          match (config.inode_config, config.store_type) with
          | `Entries_2, `Pack -> Bench_inodes_2.run_chains config
          | `Entries_32, `Pack -> Bench_inodes_32.run_chains config
          | `Entries_2, `Pack_layered ->
              Bench_inodes_2_layered.run_chains config
          | `Entries_32, `Pack_layered ->
              Bench_inodes_32_layered.run_chains config);
    };
    {
      mode = `Large;
      speed = `Custom;
      run =
        (fun config ->
          match (config.inode_config, config.store_type) with
          | `Entries_2, `Pack -> Bench_inodes_2.run_read_trace config
          | `Entries_32, `Pack -> Bench_inodes_32.run_read_trace config
          | `Entries_2, `Pack_layered ->
              Bench_inodes_2_layered.run_read_trace config
          | `Entries_32, `Pack_layered ->
              Bench_inodes_32_layered.run_read_trace config);
    };
  ]

let get_suite suite_filter =
  List.filter
    (fun { mode; speed; _ } ->
      match (suite_filter, speed, mode) with
      | `Slow, `Slow, `Read_trace ->
          (* The suite contains several `Read_trace benchmarks, let's keep the
             slow one only *)
          true
      | `Slow, _, `Read_trace -> false
      | `Slow, (`Slow | `Quick), _ -> true
      | `Quick, `Quick, _ -> true
      | `Custom_trace, `Custom, `Read_trace -> true
      | `Custom_chains, `Custom, `Chains -> true
      | `Custom_large, `Custom, `Large -> true
      | (`Slow | `Quick | `Custom_trace | `Custom_chains | `Custom_large), _, _
        ->
          false)
    suite

let main () ncommits ncommits_trace suite_filter inode_config store_type
    freeze_commit flatten depth width nchain_trees nlarge_trees commit_data_file
    results_dir =
  let default = match suite_filter with `Quick -> 10000 | _ -> 13315 in
  let ncommits_trace = Option.value ~default ncommits_trace in
  let config =
    {
      ncommits;
      ncommits_trace;
      root = "test-bench";
      flatten;
      depth;
      width;
      nchain_trees;
      nlarge_trees;
      commit_data_file;
      inode_config;
      store_type;
      freeze_commit;
      results_dir;
    }
  in
  Printexc.record_backtrace true;
  Random.self_init ();
  FSHelper.rm_dir config.root;
  let suite = get_suite suite_filter in
  let run_benchmarks () = Lwt_list.map_s (fun b -> b.run config) suite in
  let results = Lwt_main.run (run_benchmarks ()) in
  Fmt.pr "%a@." Fmt.(list ~sep:(any "@\n@\n") (fun ppf f -> f ppf)) results

open Cmdliner

let mode =
  let mode =
    [
      ("slow", `Slow);
      ("quick", `Quick);
      ("trace", `Custom_trace);
      ("chains", `Custom_chains);
      ("large", `Custom_large);
    ]
  in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "mode" ] in
  Arg.(value @@ opt (Arg.enum mode) `Slow doc)

let inode_config =
  let mode = [ ("2", `Entries_2); ("32", `Entries_32) ] in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "inode-config" ] in
  Arg.(value @@ opt (Arg.enum mode) `Entries_32 doc)

let store_type =
  let mode = [ ("pack", `Pack); ("pack-layered", `Pack_layered) ] in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "store-type" ] in
  Arg.(value @@ opt (Arg.enum mode) `Pack doc)

let freeze_commit =
  let doc =
    Arg.info
      ~doc:"Index of the commit after which to start the layered store freeze."
      [ "freeze-commit" ]
  in
  Arg.(value @@ opt int 1664 doc)

let flatten =
  let doc =
    Arg.info ~doc:"Flatten the paths in the trace benchmarks" [ "flatten" ]
  in
  Arg.(value @@ flag doc)

let ncommits =
  let doc =
    Arg.info ~doc:"Number of commits for the large and chain modes."
      [ "n"; "ncommits" ]
  in
  Arg.(value @@ opt int 2 doc)

let ncommits_trace =
  let doc =
    Arg.info ~doc:"Number of commits to read from trace." [ "ncommits_trace" ]
  in
  Arg.(value @@ opt (some int) None doc)

let depth =
  let doc =
    Arg.info ~doc:"Depth of a commit's tree in chains-mode." [ "d"; "depth" ]
  in
  Arg.(value @@ opt int 1000 doc)

let nchain_trees =
  let doc =
    Arg.info ~doc:"Number of chain trees per commit in chains-mode."
      [ "c"; "nchain" ]
  in
  Arg.(value @@ opt int 1 doc)

let width =
  let doc =
    Arg.info ~doc:"Width of a commit's tree in large-mode." [ "w"; "width" ]
  in
  Arg.(value @@ opt int 1000000 doc)

let nlarge_trees =
  let doc =
    Arg.info ~doc:"Number of large trees per commit in large-mode."
      [ "l"; "nlarge" ]
  in
  Arg.(value @@ opt int 1 doc)

let commit_data_file =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Trace of Tezos operations to be replayed" []
  in
  Arg.(required @@ pos 0 (some string) None doc)

let results_dir =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Destination of the bench artefacts."
      [ "results" ]
  in
  Arg.(value @@ opt string default_results_dir doc)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main_term =
  Term.(
    const main
    $ setup_log
    $ ncommits
    $ ncommits_trace
    $ mode
    $ inode_config
    $ store_type
    $ freeze_commit
    $ flatten
    $ depth
    $ width
    $ nchain_trees
    $ nlarge_trees
    $ commit_data_file
    $ results_dir)

let () =
  let info = Term.info "Benchmarks for tree operations" in
  Term.exit @@ Term.eval (main_term, info)
