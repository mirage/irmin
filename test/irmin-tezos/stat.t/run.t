Running stat on a v3 store
  $ STORE=PACK ../irmin_fsck.exe stat ../data/pack 2>&1 | cat -e | tail -n 10
    "hash_size": {$
      "Bytes": 64$
    },$
    "log_size": 2500000,$
    "objects": {$
      "nb_commits": 3,$
      "nb_nodes": 16,$
      "nb_contents": 3$
    }$
  }

Running index-integrity-check on a v3 store minimal
  $ STORE=PACK ../irmin_fsck.exe integrity-check-index ../data/pack 2>&1 | cat -e | tail -n 10
    { "Commit_v1" = 0;$
      "Commit_v2" = 3;$
      "Contents" = 3;$
      "Inode_v1_unstable" = 0;$
      "Inode_v1_stable" = 0;$
      "Inode_v2_root" = 12;$
      "Inode_v2_nonroot" = 4;$
      "Dangling_parent_commit" = 0;$
      "Duplicated entries" = 0;$
      "Missing entries" = 0 }$
