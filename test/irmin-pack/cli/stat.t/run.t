Running stat on a layered store after a first freeze
  $ PACK=layered ../irmin_fsck.exe stat ../data/layered_pack_upper
  >> Getting statistics for store: `../data/layered_pack_upper'
  
  	0k contents / 0k nodes / 0k commits	0k contents / 0k nodes / 0k commits
  	0k contents / 0k nodes / 0k commits	0k contents / 0k nodes / 0k commits
  	0k contents / 0k nodes / 0k commits	0k contents / 0k nodes / 0k commits
  {
    "hash_size": {
      "Bytes": 64
    },
    "log_size": 500000,
    "files": {
      "flip": "Upper0",
      "lower": {
        "pack": {
          "size": {
            "Bytes": 466
          },
          "offset": 442,
          "generation": 0,
          "version": "V2"
        },
        "branch": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0,
          "version": "V2"
        },
        "dict": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0,
          "version": "V2"
        }
      },
      "upper1": {
        "pack": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 1,
          "version": "V2"
        },
        "branch": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0,
          "version": "V2"
        },
        "dict": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0,
          "version": "V2"
        }
      },
      "upper0": {
        "pack": {
          "size": {
            "Bytes": 597
          },
          "offset": 573,
          "generation": 0,
          "version": "V2"
        },
        "branch": {
          "size": {
            "Bytes": 98
          },
          "offset": 74,
          "generation": 0,
          "version": "V2"
        },
        "dict": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0,
          "version": "V2"
        }
      }
    },
    "objects": {
      "lower": {
        "nb_commits": 1,
        "nb_nodes": 3,
        "nb_contents": 1
      },
      "upper1": {
        "nb_commits": 0,
        "nb_nodes": 0,
        "nb_contents": 0
      },
      "upper0": {
        "nb_commits": 1,
        "nb_nodes": 3,
        "nb_contents": 1
      }
    }
  }

Running check on a layered store that is not self contained

  $ PACK=layered ../irmin_fsck.exe check ../data/layered_pack_upper
  Error -- Upper layer is not self contained for heads 6a88b83f76c48b1b20a0d421c73de6e0e0e42553d44a40fccc07af074f304e0d9f10a7b5ab47b61205da0f2f599d2ebb1bf27452c05b926b0c7ef8f867778130: 2 phantom objects detected

Traverse store for stats

  $ PACK_LAYERED=false ../irmin_fsck.exe stat-store --commit=cb71b759dd9007c31f6d950122ec14d89519e965e8d68af5f4e73c1f348d97bc24dfe5127ed01dbdb5f54105e4e4548948925f423d809aea85b6d130d68d413f ../data/pack
  >> Max width = { "maximum" = 3;
                   "maximal_count" = 1;
                   "representative" = a/b1/c1 }
  >> Max number of path-adjacent nodes = { "maximum" = 3;
                                           "maximal_count" = 2;
                                           "representative" = a/b1/c1/-/d2/e2 }
  >> Max length = { "maximum" = 5;
                    "maximal_count" = 3;
                    "representative" = a/b1/c1/d1/e1 }

  $ PACK_LAYERED=false ../irmin_fsck.exe stat-store --commit=9ed4837f71df8bfd3d7b724451986698fd8ac48bf01c79df1d383ea9e041fe86fce66d1ba3e228b69b085f5b17c5f42491e22f52e08a20912c70e9d0a679cfa4 ../data/pack
  >> Max width = { "maximum" = 3;
                   "maximal_count" = 2;
                   "representative" = a }
  >> Max number of path-adjacent nodes = { "maximum" = 3;
                                           "maximal_count" = 2;
                                           "representative" = a/-/b1/c1/-/d2/e2 }
  >> Max length = { "maximum" = 5;
                    "maximal_count" = 3;
                    "representative" = a/b1/c1/d1/e1 }

  $ PACK_LAYERED=false ../irmin_fsck.exe stat-store --commit=04bc4d901a43c2efb65ddb0097a483e10acb1769f525d19e33f46cb33f6d85708c58216adc7d385152b5f6567523d0d869555d30804a37d6035607a3c938177a ../data/pack
  >> Max width = { "maximum" = 2;
                   "maximal_count" = 1;
                   "representative" = a }
  >> Max number of path-adjacent nodes = { "maximum" = 1;
                                           "maximal_count" = 2;
                                           "representative" = a/b2/c2/e3 }
  >> Max length = { "maximum" = 4;
                    "maximal_count" = 1;
                    "representative" = a/b2/c2/e3 }
