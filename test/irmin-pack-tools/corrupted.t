The pack inspection tools report the files which are not valid, instead of
stopping on an exception.

Create a store
  $ ./make_store.exe store

The tools work on a valid store
  $ irmin-ppcf Upper store/store.control > /dev/null
  $ irmin-ppidx Tezos store | sort
  {"hash":"CoV8LhTqRDPRKMxF9rT2YT986AM8Zkx5iKhWYaBM3sCNQx1Qzu3c","off":37,"len":47,"kind":"Inode_v2_root"}
  {"hash":"CoVfXqZz6qmLScDZ73f6gG7KD2rZkUXPeMpLsnpLaTQ5vCRjqBkq","off":0,"len":37,"kind":"Contents"}
  {"hash":"CoVzFXoaS53m2CHjuEBVnRstCYirw6s3dirKhh3ra76prQ1kq4Kg","off":178,"len":62,"kind":"Commit_v2"}
  {"hash":"CoW56Dt5jzxWXhkdXwQ1DzqqytN2LL8fxoCd53FnRy9p9VZtkW5E","off":84,"len":47,"kind":"Inode_v2_root"}
  {"hash":"CoWJL6uhULU5XM9kajwapoXZfVxDJ7KSjuQ9Mv9okQYaMJteEfLt","off":131,"len":47,"kind":"Inode_v2_root"}
  $ irmin-tezos-explorer parse --store_path store

An index with an unknown kind of entry
  $ cp -r store bad-index-kind
  $ printf 'Z' | dd of=bad-index-kind/index/log bs=1 seek=76 conv=notrunc 2> /dev/null
  $ irmin-ppidx Tezos bad-index-kind > /dev/null
  irmin-ppidx: the index cannot be decoded (Kind.of_magic: unexpected magic char 'Z'). Is it an index of a Tezos store?
  [1]

A truncated log of the index is not tested here: with index 1.7.0, reading
it fails on an assertion of the index library itself.

A control file which is not valid
  $ printf 'not a control file' > bad.control
  $ irmin-ppcf Upper bad.control
  irmin-ppcf: {"Unknown_major_pack_version":"not a co"}
  [1]

A pack file with an unknown kind of entry
  $ cp -r store bad-kind
  $ printf 'Z' | dd of=bad-kind/store.0.suffix bs=1 seek=32 conv=notrunc 2> /dev/null
  $ irmin-tezos-explorer parse --store_path bad-kind --store_idx bad.idx --store_info_last bad.last --store_info_next bad.next
  irmin-tezos-explorer parse: unknown kind of entry 'Z' at offset 0
  [1]

A pack file with an entry which is too long
  $ cp -r store bad-length
  $ printf '\377\377\377\377\377\377\377\377\077' | dd of=bad-length/store.0.suffix bs=1 seek=33 conv=notrunc 2> /dev/null
  $ irmin-tezos-explorer parse --store_path bad-length --store_idx bad.idx --store_info_last bad.last --store_info_next bad.next
  irmin-tezos-explorer parse: the entry at offset 0 has a length of 4611686018427387903 bytes
  [1]

A pack file with an entry of negative length
  $ cp -r store bad-negative
  $ printf '\377\377\377\377\377\377\377\377\177' | dd of=bad-negative/store.0.suffix bs=1 seek=33 conv=notrunc 2> /dev/null
  $ irmin-tezos-explorer parse --store_path bad-negative --store_idx bad.idx --store_info_last bad.last --store_info_next bad.next
  irmin-tezos-explorer parse: the entry at offset 0 has a length of -1 bytes
  [1]
