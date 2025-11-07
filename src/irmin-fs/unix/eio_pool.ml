open Eio

type 'a t = {
  create : unit -> 'a;
  (* Create a new pool member. *)
  check : 'a -> (bool -> unit) -> unit;
  (* Check validity of a pool member when use resulted in failed promise. *)
  validate : 'a -> bool;
  (* Validate an existing free pool member before use. *)
  dispose : 'a -> unit;
  (* Dispose of a pool member. *)
  cleared : bool ref ref;
  (* Have the current pool elements been cleared out? *)
  max : int;
  (* Size of the pool. *)
  mutable count : int;
  (* Number of elements in the pool. *)
  list : 'a Queue.t;
  (* Available pool members. *)
  waiters : ('a, exn) result Promise.u Stream.t;
      (* Promise resolvers waiting for a free member. *)
}

let create m ?(validate = fun _ -> true) ?(check = fun _ f -> f true)
    ?(dispose = fun _ -> ()) create =
  {
    max = m;
    create;
    validate;
    check;
    dispose;
    cleared = ref (ref false);
    count = 0;
    list = Queue.create ();
    waiters = Stream.create m;
  }

(* Create a pool member. *)
let create_member p =
  try
    (* Must be done before p.create to prevent other resolvers from
       creating new members if the limit is reached. *)
    p.count <- p.count + 1;
    p.create ()
  with exn ->
    (* Creation failed, so don't increment count. *)
    p.count <- p.count - 1;
    raise exn

(* Release a pool member. *)
let release p c =
  match Stream.take_nonblocking p.waiters with
  | Some wakener ->
      (* A promise resolver is waiting, give it the pool member. *)
      Promise.resolve_ok wakener c
  | None ->
      (* No one is waiting, queue it. *)
      Queue.push c p.list

(* Dispose of a pool member. *)
let dispose p c =
  p.dispose c;
  p.count <- p.count - 1;
  ()

(* Create a new member when one is thrown away. *)
let replace_disposed p =
  match Stream.take_nonblocking p.waiters with
  | None ->
      (* No one is waiting, do not create a new member to avoid
         losing an error if creation fails. *)
      ()
  | Some wakener -> (
      match p.create () with
      | c -> Promise.resolve_ok wakener c
      | exception exn ->
          (* Creation failed, notify the waiter of the failure. *)
          Promise.resolve_error wakener exn)

(* Verify a member is still valid before using it. *)
let validate_and_return p c =
  match p.validate c with
  | true -> c
  | false ->
      (* Remove this member and create a new one. *)
      dispose p c;
      create_member p
  | exception e ->
      (* Validation failed: create a new member if at least one
         resolver is waiting. *)
      dispose p c;
      replace_disposed p;
      raise e

(* Acquire a pool member. *)
let acquire p =
  if Queue.is_empty p.list then (
    if
      (* No more available member. *)
      p.count < p.max
    then
      (* Limit not reached: create a new one. *)
      create_member p
    else
      (* Limit reached: wait for a free one. *)
      let promise, resolver = Promise.create () in
      Stream.add p.waiters resolver;
      validate_and_return p (Promise.await_exn promise))
  else
    (* Take the first free member and validate it. *)
    let c = Queue.take p.list in
    validate_and_return p c

(* Release a member when use resulted in failed promise if the member
   is still valid. *)
let check_and_release p c cleared =
  let ok = ref false in
  p.check c (fun result -> ok := result);
  if cleared || not !ok then
    (* Element is not ok or the pool was cleared - dispose of it *)
    dispose p c
  else (* Element is ok - release it back to the pool *)
    release p c

let use p f =
  let c = acquire p in
  (* Capture the current cleared state so we can see if it changes while this
     element is in use *)
  let cleared = !(p.cleared) in
  let promise () =
    try f c
    with e ->
      check_and_release p c !cleared;
      raise e
  in
  let r = promise () in
  if !cleared then (
    (* p was cleared while promise was resolving - dispose of this element *)
    dispose p c;
    r)
  else (
    release p c;
    r)

let clear p =
  let elements = Queue.fold (fun l element -> element :: l) [] p.list in
  Queue.clear p.list;
  (* Indicate to any currently in-use elements that we cleared the pool *)
  let old_cleared = !(p.cleared) in
  old_cleared := true;
  p.cleared := ref false;
  List.iter (dispose p) elements

let wait_queue_length p = Stream.length p.waiters
