open QCheck
open STM
module Lru = Irmin.Backend.Lru.Make (Int)

(** parallel STM tests of Lru *)
module Model = struct
  type sut = int Lru.t
  type state = (int * int) list
  type cmd = Add of int * int | Find of int | Mem of int | Drop

  let show_cmd c =
    match c with
    | Add (i, j) -> "Add (" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")"
    | Find k -> "Find " ^ string_of_int k
    | Mem k -> "Mem " ^ string_of_int k
    | Drop -> "Drop"

  let init_sut () = Lru.create 42
  let cleanup (_ : sut) = ()

  let arb_cmd (_ : state) =
    let int = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun k v -> Add (k, v)) int int;
           Gen.map (fun k -> Find k) int;
           Gen.map (fun k -> Mem k) int;
           Gen.pure Drop;
         ])

  let next_state (c : cmd) (s : state) =
    match c with
    | Add (k, v) -> (k, v) :: List.remove_assoc k s
    | Drop -> ( match List.rev s with [] -> [] | _ :: xs -> List.rev xs)
    | Find k | Mem k -> (
        match List.assoc_opt k s with
        | Some v -> (k, v) :: List.remove_assoc k s
        | None -> s)

  let run (c : cmd) (h : sut) =
    match c with
    | Add (k, v) -> Res (unit, Lru.add h k v)
    | Find k -> Res (result int exn, protect (Lru.find h) k)
    | Mem k -> Res (bool, Lru.mem h k)
    | Drop -> Res (option int, Lru.drop h)

  let init_state = []
  let precond (_ : cmd) (_ : state) = true

  let postcond (c : cmd) (s : state) (res : res) =
    match (c, res) with
    | Add (_, _), Res ((Unit, _), _) -> true
    | Find k, Res ((Result (Int, Exn), _), r) -> (
        r = try Ok (List.assoc k s) with Not_found -> Error Not_found)
    | Mem k, Res ((Bool, _), r) -> r = List.mem_assoc k s
    | Drop, Res ((Option Int, _), r) -> (
        match (r, List.rev s) with
        | None, [] -> true
        | Some v, x :: _ -> v = snd x
        | _ -> false)
    | _ -> false
end

module LRU_seq = STM_sequential.Make (Model)
module LRU_dom = STM_domain.Make (Model);;

QCheck_base_runner.run_tests_main
  (let count = 200 in
   [
     LRU_seq.agree_test ~count ~name:"Lru test sequential";
     LRU_dom.agree_test_par ~count ~name:"Lru test parallel";
   ])
