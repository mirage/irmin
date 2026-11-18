let main ~sw:_ ~fs:_ _store_path _info_last_path _info_next_path _index_path =
  prerr_endline
    "irmin-tezos-explorer: the show command needs notty, which was not \
     installed when this program was built.";
  exit 1
