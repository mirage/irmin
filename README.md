## Irminsule

Irminsule is a distributed database with built-in snapshot, branch and
revert mechanisms. It is designed to use a large variety of backends,
although it is optimized for append-only store.

Irminsule is written in pure OCaml. It can thus be compiled to Javascript
-- and run in the browsers; or into a Mirage microkernel -- and run directly
on top of Xen.

### Build & Install

```
opam install ezjsonm ocamlgraph lwt cryptokit \
             re dolog mstruct core_kernel \
             uri cohttp ssl \
             core_kernel cmldiner
make
make install
```

### Usage

```
COMMANDS
       clone
           Clone a remote irminsule store.

       dump
           Dump the contents of the store as a Graphviz file.

       init
           Initialize a store.

       ls  List subdirectories.

       pull
           Pull the contents of a remote irminsule store.

       push
           Pull the contents of the local store to a remote irminsule store.

       read
           Read the contents of a node.

       revert
           Revert the contents of the store to a previous state.

       rm  Remove a node.

       snapshot
           Snapshot the contents of the store.

       tree
           List the store contents.

       watch
           Watch the contents of a store and be notified on updates.

       write
           Write/modify a node.
```

See `irmin --help` for further reading. Use either `irmin <command> --help`
 or `irmin help <command>` for more information on a specific command.

## Issues

To report any issues please use the [bugtracker on Github](https://github.com/samoht/issues).
