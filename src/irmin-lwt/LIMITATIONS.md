# `irmin-lwt` — what is not supported

`irmin-lwt` is a shim that exposes Irmin 3's Lwt-flavoured API on top of
Irmin 4's direct-style (Eio) implementation. Most of the public surface
of `Irmin` is preserved, but a few entry points are intentionally not
forwarded. This file is the canonical list.

## Not exposed

### `Of_backend`

`Irmin.Of_backend (B : Backend.S)` lets a user supply a hand-rolled
`Backend.S` (Contents store, Node store, Commit store, Branch store,
Slice, Remote, Repo) and obtain a full Irmin store.

`irmin-lwt` does **not** expose `Of_backend`. Routing a Lwt-typed
`Backend.S` through Irmin 4 would require an extensive Lwt → Eio adapter
for every sub-store (~400 lines of bridges with per-operation runtime
overhead). No application user of `irmin-lwt` needs this entry point —
in the wider Irmin codebase, only `irmin-git`, `irmin-client`, and
`irmin-pack-mem` use `Of_backend`, none of which are in the `irmin-lwt`
roadmap.

If you really need to plug a custom backend into the Lwt API:

- implement your backend against `Irmin`'s direct-style `Backend.S`
  (Eio-typed) and apply `Irmin.Of_backend`, then wrap the resulting
  `Generic_key.S` back to the Lwt surface using
  `Irmin_lwt.Wrap_store.Make`; or
- use the higher-level supported entry points: `Maker` or
  `Storage.Make`.

### `Generic_key.Maker` (the functor)

`Irmin.Generic_key.Maker (X)` takes four per-store sub-Makers
(`Contents_store`, `Node_store`, `Commit_store`, `Branch_store`) — each
a `Maker` parameterised by hash and value type — and produces a `Maker`
keyed by whatever the user-supplied sub-Makers want. Used by backends
with non-hash keys like `Pack_key.t`.

`irmin-lwt` exposes the `Generic_key.Maker` **module type** (it is the
public contract that backends like `irmin-lwt-pack` declare against)
but not the **functor**. Routing user-supplied Lwt sub-Makers through
Irmin 4 forces every store operation through a Lwt → Eio → Lwt
round-trip. No application user needs the functor: in the wider Irmin
codebase only two test files invoke it directly, and `irmin-lwt-pack`
satisfies the signature by hand using `Wrap_store.Make`.

Backend authors with custom keyed Lwt sub-Makers should:

- implement against `Irmin`'s direct-style `Generic_key.Maker (X)`
  (Eio-typed) and lift the result back to the Lwt surface with
  `Wrap_store.Make`; or
- follow the `irmin-lwt-pack.Maker` pattern: write a hand-rolled
  `Maker` that satisfies `Generic_key.Maker` and delegates to
  `Wrap_store.Make` internally.

### `Store.Make` and `Tree.Make` are gone

The two implementation functors `Store.Make (B : Backend.S)` and
`Tree.Make (B : Backend.S)` (~4000 lines combined, verbatim from
Irmin 3 / `main`) used to live in `core/store.ml` and `core/tree.ml`.
Both have been deleted: with `Of_backend` and `Generic_key.Maker`
removed, neither functor had any consumer. All Lwt-typed stores now
flow through `Wrap_store.Make`, which delegates the tree machinery to
Irmin 4's `Tree` via `Inner.Tree`. The module **types** `Store.S`,
`Store.KV`, `Store.Maker`, `Store.KV_maker`, `Store.Generic_key.S`,
`Store.Json_tree` and `Tree.S` are still exposed.

## Caveats / things that work but with constraints

### `Watch.set_watch_switch`

Irmin 4's watch infrastructure forks an Eio fiber to dispatch events,
and that fiber needs an Eio switch. `Irmin.Backend.Watch.set_watch_switch`
must be called once before any watch operation. Irmin itself does not
call it automatically (the comment in `watch.ml` reads *"a terrible
hack that will need fixed"*). If you use watches through `irmin-lwt`,
you must call this from your runner:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
Irmin.Backend.Watch.set_watch_switch sw;
Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
…
```

### Packages not yet ported to the Lwt shim

The following Irmin sister packages are not currently shipped as
`irmin-lwt-*` shims. They are all bigger / more specialised than the
core backends and are deferred until a concrete user demands them.
None block application code: users who need them can call the Eio
counterpart directly from a Lwt program via `Lwt_eio.run_eio`, with
the trade-off that the backend store will not be the same `Repo.t`
the rest of the application uses.

- **`irmin-graphql`** — already Lwt-typed (cohttp-lwt + graphql-lwt)
  internally bridged to an Eio store via `Lwt_eio.run_eio`. To wrap
  it on a Lwt-typed store would require exposing the underlying Eio
  store of each `irmin-lwt-*` backend (the `Inner` parameter of
  `Wrap_store.Make`), which the current `.mli` files seal.
- ~~**`irmin-git`**~~ — **shipped** as `irmin-lwt-git` (commit
  `4160559c59`).
- **`irmin-mirage`** (core) — single helper module ([Info]); trivial
  to port whenever needed.
- **`irmin-mirage-graphql`** — uses cohttp-lwt; would inherit from
  whatever shape `irmin-graphql` ends up taking.
- **`irmin-mirage-git`** — interestingly, this is **still Lwt-typed
  in its public API** (`connect : ... -> t Lwt.t`, `batch : ... -> 
  ('a Lwt.t) -> 'a Lwt.t`), because Mirage's `Mirage_kv.RO` /
  `Mirage_kv.RW` types are defined in Lwt. Lwt users wanting a Mirage
  Git store can use it directly today — no shim needed.
- ~~**`irmin-tezos`**~~ — **shipped** as `irmin-lwt-tezos` (commit
  `52fdaf114f`). Note: the Tezos team is *not* on Eio; Tezos uses
  `irmin-pack` directly. The Lwt shim preserves the V1 pre-hashing
  end-to-end so on-disk data stays wire-compatible with regular
  `irmin-tezos` data.
- **`irmin-cli`** — command-line utilities (binary, not a library used
  from application code). Lwt vs Eio is irrelevant.
- ~~**`irmin-client`**~~ — **shipped** as `irmin-lwt-client`
  (commit `0496a7c609`). The server (`irmin-server`) is standalone
  and does not need a Lwt-flavoured shim — Lwt apps use
  `irmin-lwt-client` to connect to an Eio server running on the
  network.

If you need any of these on the Lwt API, please open an issue.

### `irmin-lwt-pack` advanced features

The pack backend exposes the full `irmin-pack-unix` surface, with each
Eio-direct effectful operation bridged to `Lwt.t` via `Lwt_eio.run_eio`:

- Integrity checks (`integrity_check`, `integrity_check_inodes`,
  `traverse_pack_file`, `test_traverse_pack_file`)
- Chunking / lower layer / on-disk (`split`, `is_split_allowed`,
  `add_volume`, `reload`, `flush`, `create_one_commit_store`)
- Statistics (`stats`)
- Garbage collection (`Gc.start_exn`, `finalise_exn`, `run`, `wait`,
  `cancel`, `is_finished`, `behaviour`, `is_allowed`,
  `latest_gc_target`)
- Snapshots (`Snapshot.export`, `Snapshot.Import.{v,save_elt,close}`)

A few entry points unavoidably leak Eio types: those that take Eio
capabilities as arguments — `~domain_mgr:_ Eio.Domain_manager.t` (for
`Gc.start_exn`, `Gc.run`, `create_one_commit_store`) and
`Eio.Fs.dir_ty Eio.Path.t` (for `create_one_commit_store`,
`Snapshot.export ?on_disk`, `Snapshot.Import.v ?on_disk`). The shim
does not hide Eio entirely; Lwt callers must obtain these from their
top-level `Eio_main.run` runner (typically
`Eio.Stdenv.domain_mgr env` and `Eio.Stdenv.fs env`).

The `Internal` sub-module of `Irmin_pack_unix.S` (unstable, used only
for inode tests) is deliberately not forwarded.
