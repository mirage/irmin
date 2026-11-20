The show command of irmin-tezos-explorer reports the index files which are
not valid, instead of stopping on an exception.

Create a store
  $ ./make_store.exe store
  $ irmin-tezos-explorer parse --store_path store

An index of entries announcing more entries than it holds
  $ printf '\377\377\377\377\377\377\377\377\077' > bad.idx
  $ irmin-tezos-explorer show --store_path store --store_idx bad.idx
  irmin-tezos-explorer show: bad.idx announces 4611686018427387903 entries
  [1]

An index of entries announcing a negative number of entries
  $ printf '\377\377\377\377\377\377\377\377\177' > bad.idx
  $ irmin-tezos-explorer show --store_path store --store_idx bad.idx
  irmin-tezos-explorer show: bad.idx announces -1 entries
  [1]

An index of entries which is truncated
  $ printf '\002\001\001' > bad.idx
  $ irmin-tezos-explorer show --store_path store --store_idx bad.idx
  irmin-tezos-explorer show: bad.idx announces 2 entries
  [1]
  $ printf '\001\001\001\377' > bad.idx
  $ irmin-tezos-explorer show --store_path store --store_idx bad.idx
  irmin-tezos-explorer show: bad.idx is truncated at offset 1
  [1]
