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
          "generation": 0
        },
        "branch": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0
        },
        "dict": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0
        }
      },
      "upper1": {
        "pack": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 1
        },
        "branch": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0
        },
        "dict": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0
        }
      },
      "upper0": {
        "pack": {
          "size": {
            "Bytes": 597
          },
          "offset": 573,
          "generation": 0
        },
        "branch": {
          "size": {
            "Bytes": 98
          },
          "offset": 74,
          "generation": 0
        },
        "dict": {
          "size": {
            "Bytes": 24
          },
          "offset": 0,
          "generation": 0
        }
      }
    }
  }

Running check on a layered store that is not self contained

  $ PACK_LAYERED=true ../irmin_fsck.exe check ../data/layered_pack_upper
  Error -- Upper layer is not self contained for heads ff696e4f06f0b156e542c77d8b35a8419158b00c22168154a4b004684e36dbe8cd3e30de3b9e2c7e2d1acd07bed152bd65a79c349e4aacccdb379ea621e9be40: 2 phantom objects detected
