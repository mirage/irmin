Running stat on a layered store after a first freeze
  $ PACK_LAYERED=true ../irmin_fsck.exe stat ../data/layered_pack_upper
  >> Getting statistics for store: `../data/layered_pack_upper'
  
  {
    "hash_size": {
      "Bytes": 64
    },
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
    }
  }

Running check on a layered store that is not self contained

  $ PACK_LAYERED=true ../irmin_fsck.exe check ../data/layered_pack_upper
  Error -- Upper layer is not self contained for heads 174ab7219d81baf9317ff892fa9c767e4f1487759b5748dbefb940802f2bde381a1f9129c3632b3265205b7e105b39eaeded3e25e21c6b053ce8ccb44cd887f0: 2 phantom objects detected
