# Irmin-pack-tools
This folder contains several tools meant to provide usefull ways to debug and dump informations about stores.

Currently, there are the following tools:
- [`ppcf`](#ppcf), a json printer for control files
- [`ppidx`](#ppidx), a json printer for index folders
- [`tezos-explorer`](#tezos-explorer), a gui for a fast exploration of tezos stores

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
