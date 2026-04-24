(* TSan stress suite.

   Dispatcher for race-hunting scenarios. Each scenario targets a mutable
   state hotspot that the standard test suite does not exercise across
   domains. Scenarios are added incrementally.

   Iteration count: [IRMIN_TSAN_STRESS_ITER] env var (default 100). *)

let iter_count =
  match Sys.getenv_opt "IRMIN_TSAN_STRESS_ITER" with
  | Some s -> int_of_string s
  | None -> 100

let scenarios : (string * (iter:int -> unit)) list = []

let run_all () =
  List.iter
    (fun (name, fn) ->
      Printf.printf "tsan-stress: %s (iter=%d)\n%!" name iter_count;
      fn ~iter:iter_count)
    scenarios

let () =
  let which = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "all" in
  match which with
  | "all" -> run_all ()
  | name -> (
      match List.assoc_opt name scenarios with
      | Some fn -> fn ~iter:iter_count
      | None ->
          Printf.eprintf "tsan-stress: unknown scenario %S\n%!" name;
          exit 2)
