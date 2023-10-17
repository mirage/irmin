# Irmin-pack-tools
This folder contains several tools meant to provide usefull ways to debug and dump informations about stores.

Currently, there are the following tools:
- [`ppcf`](#ppcf), a json printer for control files
- [`ppidx`](#ppidx), a json printer for index folders
- [`tezos-explorer`](#tezos-explorer), a notty ui for a fast exploration of tezos stores
- [`tezos-explorer-gui`](#tezos-explorer-gui), a graphical ui for a fast exploration of tezos stores

## ppcf
This tool prints a control file in a human readable manner (json), allowing to fetch important informations easily.
It is fairly straightforward:
```shell
$ dune exec -- irmin-ppcf Upper|Volume <path-to-control-file>
```
The first argument, either `Upper` or `Volume` tells the tool if you are trying to read the control file of a normal store, or from a volume store.
The second one is the path to the control file (e.g. `output/root/store.control`)

It will typically give the following json output:
```
{"Valid":{"V5":{"dict_end_poff":1548250,"appendable_chunk_poff":9056699,"checksum":2724660738,"chunk_start_idx":1,"chunk_num":5,"volume_num":0,"status":{"Gced":{"suffix_start_offset":35654891,"generation":5,"latest_gc_target_offset":35654787,"suffix_dead_bytes":20655801}}}}}
```
Which can be further improved using the tool `jq`:
```shell
$ dune exec -- irmin-ppcf Upper|Volume <path-to-control-file> | jq
```
Will give:
```json
{
  "Valid": {
    "V5": {
      "dict_end_poff": 1548250,
      "appendable_chunk_poff": 9056699,
      "checksum": 2724660738,
      "chunk_start_idx": 1,
      "chunk_num": 5,
      "volume_num": 0,
      "status": {
        "Gced": {
          "suffix_start_offset": 35654891,
          "generation": 5,
          "latest_gc_target_offset": 35654787,
          "suffix_dead_bytes": 20655801
        }
      }
    }
  }
}
```

## ppidx
This tool prints the informations stored in the index of a store in a human readable manner (json), allowing to fetch important informations easily.
It is fairly straightforward:
```shell
$ dune exec -- irmin-ppidx Irmin|Tezos <path-to-store>
```
The first argument is the type of store, either:
- Irmin, which implies that we will use the SHA256 hash, and will print it has an hexadecimal representation of the hash.
- Tezos, which implies that we will use the default tezos hash (Blake2B) and we will print it with their encoding (base58 + a tezos specific prefix).

The second one is the path to the root of the store, not the index folder (e.g. `output/root/` where `output/root/index/` exists)

It will typically give the following json output for Irmin, with one line per entry registered:
```
{"hash":"706ac3565424cc48e5c360c0e1b2f97c09c348e8784d12ad2d9ac089abdc96c2","off":13431448,"len":101,"kind":"Commit_v2"}
{"hash":"8b62e251b73b7cf32ee4e37530b37deed85fedc5343944dc0c1c0d96fcc0515a","off":95461096,"len":103,"kind":"Commit_v2"}
{"hash":"71730dda7f117ba440bd56c76f36a9b737c8745a7e5ff7b2dfa4b9462593b14c","off":28541134,"len":104,"kind":"Commit_v2"}
...
```
Which can be further improved using the tool `jq`:
```shell
$ dune exec -- irmin-ppidx <path-to-store> > index
$ jq -s '.' -- index
```
Will give:
```json
{
  "hash": "706ac3565424cc48e5c360c0e1b2f97c09c348e8784d12ad2d9ac089abdc96c2",
  "off": 13431448,
  "len": 101,
  "kind": "Commit_v2"
}
{
  "hash": "8b62e251b73b7cf32ee4e37530b37deed85fedc5343944dc0c1c0d96fcc0515a",
  "off": 95461096,
  "len": 103,
  "kind": "Commit_v2"
}
{
  "hash": "71730dda7f117ba440bd56c76f36a9b737c8745a7e5ff7b2dfa4b9462593b14c",
  "off": 28541134,
  "len": 104,
  "kind": "Commit_v2"
}
...
```

However, it might be useful to sort the json output using the offset. You can do so by specifying a sort function:
```shell
$ jq -s 'sort_by(.off)' -- index
```

## tezos-explorer
TODO

## tezos-explorer-gui
This tool is a graphical UI, meant to allow the user to figure out rapidly the shape of a commit, giving them informations on its content.
In order to launch it, uncomment the `dune` file under the path `src/irmin-pack-tools/tezos_explorer_gui` and install the deps `tsdl` and `tsdl-ttf`.
You will also need to pin the package `prettree` with `git+https://github.com/art-w/prettree.git#568de08442f02dd87acc84ca6a91cc661b7e77bf`.
It can be launched using the following command:
```shell
$ dune exec -- irmin-tezos-explorer-gui <path-to-store> <path-to-ttf-font> <commit>
```

The first argument is the path to the root of the store (e.g. `output/root/`).

The second argument is the path to a `.ttf` file, necessary to know which font to use when printing strings.

The third argument is an int, the `nth` commit stored in the index of the store that will be shown first.

Once the program is launched, you can:
- Navigate through the indexed commits using the left and right arrows.
- Zoom in and out using the mouse wheel.
- Drag the tree around when pressing the left mouse click and moving it around.

Be aware that some commit are too big to be shown, and will take ages to compute for very little informations: You can shut the program down using the `alt-f4`` command.
