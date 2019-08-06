include Dict.Make (IO.Unix)

(* Add IO caching around Dict.v *)
let (`Staged v) =
  let v_no_cache ~fresh ~shared:_ ~readonly = v ~fresh ~readonly in
  IO.with_cache ~clear ~v:(fun capacity -> v_no_cache ~capacity) "store.dict"

let v ?fresh ?readonly ?shared ?(capacity = 100_000) root =
  v capacity ?fresh ?shared ?readonly root
