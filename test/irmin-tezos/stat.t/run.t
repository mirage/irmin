Running stat on a v3 store
  $ STORE=PACK ../irmin_fsck.exe stat ../data/pack
  >> Getting statistics for store: `../data/pack'
  
  mits": 3,
      "nb_nodes": 16,
      "nb_contents": 3
    }
  }

Running index-integrity-check on a v3 store minimal
  $ STORE=PACK ../irmin_fsck.exe integrity-check-index ../data/pack
  >> Beginning index checking with parameters: { log_size = 2500000 }
  in-fsck: internal error, uncaught exception:
              (Failure "Kind.of_magic: unexpected magic char '\\000'")
              
  [125]
