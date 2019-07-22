include Dict.Make
          (IO.Unix)
          (struct
            let to_bin_string i =
              Int32.of_int i |> Irmin.Type.(to_bin_string int32)

            let decode_bin s i =
              let _, v = Irmin.Type.(decode_bin int32) ?headers:None s i in
              Int32.to_int v
          end)

(* Add IO caching around Dict.v *)
let v ?fresh ?readonly ?(capacity = 100_000) root =
  let v_no_cache ~fresh ~shared:_ ~readonly = v ~fresh ~readonly in
  let (`Staged v) =
    IO.with_cache ~clear ~v:(fun capacity -> v_no_cache ~capacity) "store.dict"
  in
  v capacity ?fresh ?readonly root
