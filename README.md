## Irminsule

Irminsule is a distributed filesystem and block store that follows the
same design principles as Git. The design consists of three main
components:

* a low-level immutable and consistent key/value data-store
* a DAG persisted in that datastore; and
* a tag store which associate names to keys of the low-level data-store.

Irminsule is written in pure OCaml, and can thus be compiled to a
variety of backends (including Javascript, and Mirage
microkernels). Unlike the git frontend, applications can directly
iterate over the object graph.

The immutability of the low-level block store makes it significantly
easier to apply replication and network coding techniques to improve
resilience via replication, and to optimise scheduling across many
hosts using MPTCP-style congestion control.

### Compile

```
opam install jsonm uri ocamlgraph cohttp cmdliner obuild lwt
make
```

### Usage

* [API.ml](https://github.com/samoht/irminsule/blob/master/src/irminsule/API.ml)
  describes the API which is now more or less stabilized.

* [memory.ml](https://github.com/samoht/irminsule/blob/master/src/irminsule/memory.ml)
  is a very simple in-memory implementation of the irminsule API.

* [irminsule.ml](https://github.com/samoht/irminsule/blob/master/src/irminsule.ml)
  is a simple command-line tool to drive in-memory irminsule
  instances. You can start a dummy irminsule instance using:

  ```
  irminsule start . -x .git
  ```

  This will scan the filesystem from your current working directory,
  excluding the `.git` folder and start a daemon holding an in-memory
  image of that filesystem. You can command the daemon by connecting
  to `http://localhost:8081` (use `-p <PORT>` to change `8081` to
  something else). For instance, `http://localhost:8081/key` will
  display all the keys in the database,
  `http://localhost:8081/key/<SHA1>` will display the value associated
  to this key.

  You can then try to modify few files in your filesystem and start a
  new server by doing:

  ```
  irminsule start . -x .git -p 8082
  ```

  You can now try to pull/push between the two instance:

  ```
  irminsule pull -i 8082 -o 8081
  ````

  And the diff will be exchanged between the old server and the new
  one using a proxy.

### TODO

* check if push/pull works
* improve the exchange of keys using discover
* implement watch
* xenstore-like API
* benchmark using xstest

### Issues

SHA1 is not immune to collisions, but
[Linus](http://stackoverflow.com/questions/9392365/how-would-git-handle-a-sha-1-collision-on-a-blob/9392525#9392525)
answer is still relevant in the case of irminsule. The only problem
being that the *inadvertent kind* needs to be handle properly and
reported back to the user.