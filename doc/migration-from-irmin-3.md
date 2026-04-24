# Migrating from Irmin 3 (Lwt) to Irmin 4 (Eio)

Irmin 4 swaps Lwt-based cooperative concurrency for direct-style Eio. The
core API change is straightforward on paper — every function that used
to return `'a Lwt.t` now returns `'a` directly — but rewriting a large
Irmin 3 codebase in one go is expensive.

The **`irmin-lwt`** package provides a thin compatibility layer that
lets you keep the Irmin 3 Lwt-monadic style while the backend is already
Irmin 4. It is built on top of
[`lwt_eio`](https://github.com/ocaml-multicore/lwt_eio): every wrapped
operation crosses the Lwt/Eio bridge via `Lwt_eio.run_eio`.

This document describes a two-step migration path:

1. Move to `irmin-lwt` with minimal code changes. This unblocks Irmin 4
   adoption without rewriting every call site at once.
2. At your own pace, replace `irmin-lwt` calls with direct-style
   `Irmin` calls. This can happen module by module.

## Step 1: switch to `irmin-lwt`

### Update the opam dependencies

```diff
 depends: [
-  "irmin"       {>= "3.0"}
-  "irmin-pack"  {>= "3.0"}
+  "irmin"       {>= "4.0.0"}
+  "irmin-pack"  {>= "4.0.0"}
+  "irmin-lwt"   {>= "4.0.0"}
   "lwt"
 ]
```

You can still depend on `lwt` directly. `irmin-lwt` is built on top of
it, not a replacement.

### Instantiate the store through `Irmin_lwt.Make`

```diff
-module Store = Irmin_pack_unix.KV (Irmin.Contents.String)
+module Store4 = Irmin_pack_unix.KV (Irmin.Contents.String)
+module Store = Irmin_lwt.Make (Store4)
```

`Store.t`, `Store.repo`, `Store.tree`, `Store.commit` and so on are all
re-exported unchanged from the underlying backend. The difference is
that `Store.Repo.v`, `Store.find`, `Store.set_exn`, `Store.merge_into`,
etc. now return `_ Lwt.t` instead of direct values.

### Replace the entry point

```diff
-let () = Lwt_main.run (main ())
+let () = Irmin_lwt.run main
```

`Irmin_lwt.run` wraps `Eio_main.run` + `Lwt_eio.with_event_loop` + the
call to your `main`. It is the single line of Eio awareness a migrated
program needs at the top.

If your program is already running inside an Eio event loop (for
example, you are writing a library that receives an `env` from its
caller), use `Irmin_lwt.run_with_env env main` instead.

### Leave the rest alone

Every other call site stays the same. `let*`, `>>=`, `Lwt.return`,
`Lwt.catch`, `Lwt.fail`, `Lwt.pick`, `Lwt.async` all work because
`irmin-lwt` returns `'a Lwt.t`.

### A minimal before/after

**Irmin 3 (Lwt):**

```ocaml
open Lwt.Syntax
module Store = Irmin_mem.KV (Irmin.Contents.String)

let info = Irmin_mem.Info.none

let main () =
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info [ "hello" ] "world" in
  let* v = Store.find t [ "hello" ] in
  Lwt.return (Option.value ~default:"(none)" v)

let () =
  let result = Lwt_main.run (main ()) in
  print_endline result
```

**Migrated via `irmin-lwt`:**

```ocaml
open Lwt.Syntax
module Store4 = Irmin_mem.KV.Make (Irmin.Contents.String)
module Store = Irmin_lwt.Make (Store4)

let info message () = Store4.Info.v ~author:"app" ~message 0L

let main () =
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:(info "seed") [ "hello" ] "world" in
  let* v = Store.find t [ "hello" ] in
  Lwt.return (Option.value ~default:"(none)" v)

let () =
  let result = Irmin_lwt.run main in
  print_endline result
```

The diff is confined to: the opam file, the module instantiation, the
info constructor, and the entry point. The rest of `main` is byte-for-
byte identical.

## Pitfalls not strictly related to `irmin-lwt`

These are Irmin 3 → 4 breaking changes that `irmin-lwt` cannot hide,
because they are semantic rather than monadic:

- **OCaml 5.1+ is required.** Irmin 4 uses effects; older compilers do
  not support them.
- **Configuration renames.** Some `Irmin.Backend.Conf` keys were renamed
  or dropped between 3.x and 4.x; check your `Irmin_pack.config` or
  `Irmin_git.config` call site.
- **Removed APIs.** Functions deprecated in Irmin 3.x were dropped in 4.
  Consult `CHANGES.md` for the exact list.
- **Info constructors.** `Irmin.Info.default` replaces the old
  `Irmin_unix.Info`. `Store.Info.v ~author ~message timestamp` is the
  canonical way to build a commit info.
- **Yield points.** Every `irmin-lwt` call crosses `Lwt_eio.run_eio`,
  which is a scheduler yield. If your code assumed no Lwt yield could
  happen between a sequence of Irmin calls, there is one now. This is
  almost always invisible, but it is worth knowing for subtle
  concurrency-sensitive code.

## Step 2: drop `irmin-lwt`

When a module is ready to go fully direct-style:

1. Replace `Store = Irmin_lwt.Make (Store4)` with `Store = Store4` (or
   inline the backend directly).
2. Remove the `Lwt.t` types from the local signatures.
3. Rewrite `let*` / `>>=` chains into plain sequencing (`;`) and `let`.
4. Drop `Lwt.return` wrappers.

Because this is local to a single module, it can be done piecemeal.
Callers that are still `Lwt`-monadic can keep using the module through
a thin local wrapper, or the other way around if the module exports
direct-style only.

Once no caller needs the Lwt wrapping, you can remove the `irmin-lwt`
dependency and switch the entry point back to `Eio_main.run`.

## Scope of `irmin-lwt`

The initial release wraps the top-level `Store` operations (`Repo`,
`main`, `of_branch`, `of_commit`, `find`, `get`, `mem`, `find_tree`,
`get_tree`, the `set`/`set_tree`/`remove` families, `merge_into`,
`last_modified`). It is enough for the most common Irmin 3 client code.

Submodules like `Tree`, `Commit`, `Branch`, `Head`, `Sync` are **not
wrapped yet**: if your code calls e.g. `Tree.add` in Lwt context, you
will need to wrap the call yourself:

```ocaml
let tree' = Lwt_eio.run_eio (fun () -> Store4.Tree.add tree path v)
```

These submodule wrappers will be added in follow-up releases based on
concrete migration feedback.
