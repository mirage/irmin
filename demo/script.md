## Setup

```shell
git clone -b eio https://github.com/mirage/irmin.git
cd irmin
opam switch create -y .
dune build
dune runtest
dune build @bench
```

## Basics

```shell
cd demo
```

By default, file `irmin.yml` contains the store's configuration. We're using the file system store `fs` and the directory `store1` as root.
```shell
echo "store: fs\nroot: bus" > irmin.yml
dune exec -- irmin set foo/bar 123
dune exec -- irmin get foo/bar
dune exec -- irmin set foo/baz 456
dune exec -- irmin get foo/baz
dune exec -- irmin list
dune exec -- irmin list foo
dune exec -- irmin remove foo/baz
dune exec -- irmin list foo
```

Rollback is native in Irmin.
```shell
dune exec -- irmin log --plain
dune exec -- irmin revert
```

Branching and merging are too.
```shell
dune exec -- irmin branches
dune exec -- irmin merge --branch alt main
dune exec -- irmin branches
dune exec -- irmin set --branch alt qux 789
dune exec -- irmin set --branch main quy 1011
dune exec -- irmin list --branch alt
dune exec -- irmin list --branch main
dune exec -- irmin merge alt
dune exec -- irmin list
```

## OCaml API

Updating in OCaml, reading using the command line.
```shell
dune build
dune exec -- ./set_pi.exe
dune exec -- irmin get -s fs -c json-value --root math pi
```

Updating using the command line, reading in OCaml.
```shell
dune exec -- irmin set -s fs -c json-value --root math tau '{ "val": 6.28 }'
dune exec -- ./get_tau.exe
```

### GraphQL

Spawn the server.
```shell
dune exec -- irmin graphql --store fs -c json-value --root math --address localhost --port 8888 &
```

Querying the server.
```
curl -s -X POST http://localhost:8888/graphql -H 'Content-Type: application/graphql'  -d 'query { main { tree { get(path: "tau") } } }' | jq ".data.main.tree.get | fromjson"
```

Pushing data to the server.
```shell
curl -s -X POST http://localhost:8888/graphql -H 'Content-Type: application/graphql' -d 'mutation { set(path: "pi", value:"{ \"val\": 3.14159265359 }") { hash } }'
dune exec -- irmin get -s fs -c json-value --root math pi | jq
```

## Benchmarks

```shell
cd ..
dune exec test/irmin-mem/bench.exe
dune exec bench/irmin/data/bench_fixed_size_string_set.exe
dune exec bench/irmin-pack/main.exe
dune exec -- test/irmin-pack/bench_multicore/main.exe cold --runs 1 --elements 10000
```

## MirageOS

```shell
qemu-system-aarch64 \
  -machine virt \
  -cpu cortex-a57 \
  -m 2048 \
  -kernel bench/mirage/hello.qemu \
  -nographic \
  -netdev user,id=net0 \
  -device virtio-net-device,netdev=net0
```

## Energy Consumption

```
cd test/irmin-pack/bench_multicore
./bench.sh -r 1 > bench.csv
make bench.png
```

