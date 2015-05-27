## 0.9.5
* Fix `Irmin.export` for the HTTP backend (#196, patch from Alex Zatelepin)
* Fix a race in `Irmin.export` (#196, patch from Alex Zatelepin)
* Add `Task.empty` (the empty task) and `Task.none` (the empty task constructor)
* Completely rewrite the notification mechanism. All the watch functions now
  take a callback as argument and return a de-allocation function. The callbacks
  receive a heads values (the last and current ones) and diff values. (#187)
  - Add `Irmin.watch_head` to watch for the changes of the current branch's head
  - Add `Irmin.watch_tags` to watch for the changes of all the tags in the store
  - Add `Irmin.watch_key` to watch for the changes of the values associated to a
    given key (this is not recursive anymore).
  - Add `View.watch_path` to watch for the changes in a subtree. The function
    return views and the user can use `View.diff` to compute differences between
    views if needed.
* Transfer the HTTP client task to the server to make the commit messages
  relative to the client state (and not the server's) (#136)
* Fix `View.remove` to clean-up empty directories (#190)
* Fix the ordering of tree entries in the Git backend (#190)
* Allow to create a new head from a view and a list of parents with
  `View.make_head` (#188)
* Allow to create an empty temporary branch with `Irmin.empty` (#161)
* Use a pure OCaml implementation of SHA1, do not depend on nocrypto anymore
  (#183, by @talex5)
* Remove `Irmin.Snapshot`. Nobody was using it and it can be easily replaced by
  `Irmin.head`, `Irmin.watch_head` and `Irmin.update_head`.
* Change signature of `Irmin.iter` to include the values and move it into
  the `Irmin.RO` signature.
* Add `Irmin.fast_forward_head` (#172)
* Add `Irmin.compare_and_set_head` (#171)
* Simplify the RW_MAKER signature (#158)
* Fix Irmin_git.RW_MAKER (#159)
* Improve the efficiency of the LCA computation (#174, with @talex5 help)
* By default, explore the full graph when computing the LCAs. The previous
  behavior was to limit the depth of the exploration to be 256 by default.

## 0.9.4
* Ensure that `Irmin.update` and `Irmin.merge` are atomic.
* Fix `Irmin.clone` of an empty branch
* Add `Irmin.RW.compare_and_test` that the backends now have to implement
  to guarantee atomicity of Irmin's high-level operations.
* Add `Irmin.Private.Lock` to provide per-handler, per-key locking. This
  can be used by backend to implement simple locking policies.
* Add `Lwt.t` to the return type of `Irmin.tag` and `Irmin.tag_exn`
* Do not throw [Not_found]. Now all the `_exn` function raise `Invalid_argument`
  (#144)
* Remove `Irmin.switch` and `Irmin.detach`
* Add `Irmin.history` to get the branch history as a DAG of heads (#140).
* Fix performance of lcas computation (#160)
* Add `Irmin.Merge.promise` combinators

## 0.9.3
* Fix the invalidation of the view caches (report by @gregtatcam).
  This was causing some confusing issues where views' sub-keys where
  not properly updated to to their new values when the view is merged
  back to the store. The issues is a regression introduced in 0.9.0.
* Add post-commit hooks for the HTTP server.
* Add `Irmin.watch_tags` to monitor tag creation and desctructions.
* Fix `Irmin.push`
* Add `Irmin.with_hrw_view` to easily use transactions.
* Add a phantom type to `Irmin.t` to denote the store capabilities
  read-only, read-write or branch-consistent.
* The `~old` argument of a merge function can now be optional to
  signify that there is no common ancestor.
* Expose `Irmin.with_rw_view` to create a temporary, in-memory and
  mutable view of the store. This can be used to perform atomic
  operations in the store (ie. non-persistent transactions).
* Simplify the view API again
* Expose the task of previous commits. This let the user access
  the Git timestamp and other info such as the committer name (#90)
* The user-defined merge functions now takes an `unit -> 'a result
  Lwt.t` argument for `~old` (instead of `'a`). Evalutating the
  function will compute the least-common ancestors. Merge functions
  which ignore the `old` argument don't have to pay the cost of
  computing the lcas anymore.
* Expose `S.lca` to get the least common ancestors
* Update to ocaml-git 1.4.6

## 0.9.2 (2015-01-19)
* Fix `S.of_head` for the HTTP client (regression introduced in 0.9.0)
* Fix regression in displaying the store's graph over HTTP introduced by
  0.9.0.
* Fix regression in watch handling introduced in 0.9.0.
* Fix regressions in `Views` introduced in 0.9.0. (thx @buzzheavyyear for
  the report)
* Always add a commit when calling a update function (`Irmin.update`
  `Irmin.remove`, `Irmin.remove_rec`) even if the contents' store have
  not changed.
* The [head] argument of [Git_unix.config] now has a proper type.
* Expose synchronisation functions for basic Irmin stores.
* The user-provided merge function now takes optional values. The
  function is now called much more often during recursive merges
  (even if one of the 3 buckets of the 3-way merge function is not
  filled -- in that case, it uses `None`).
* Also expose the type of the keys in the type basic Irmin stores. Use
  `('key, 'value) Irmint.t` instead of `'value Irmin.t`.
* The user-defined `merge` functions now take the current filename being
  merged as an additional argument.
* The user-defined `Contents` should expose a `Path` sub-module. Keys of
  the resulting Irmin store will be of type `Path.t`.
* Fix `irmin init --help`. (#103)

## 0.9.1 (2014-12-26)
* Port to Cohttp 0.14.0+ HTTP interface (#102)

## 0.9.0 (2014-12-20)
* Improve the efficiency of the Git backend
* Expose a cleaner API for the Unix backends
* Expose a cleaner public API
* Rename `Origin` into `Task` and use it pervasively through the API
* Expose a high-level REST API over HTTP (#80)
* Fix the Git backend to stop constantly overwrite `.git/HEAD` (#76)
* Add a limit on concurrently open files (#93, #75)
* Add `remove_rec` to remove directories (#74, #85)
* Remove dependency to `core_kernel` (#22, #81)
* Remove dependency to `cryptokit and `sha1` and use `nocrypto` instead
* Remove dependency to caml4
* Fix writing contents at the root of the store (#73)
* More efficient synchronization protocol between Irmin stores (#11)

## 0.8.3 (2014-06-25)
* Views now keep track of their parent commits - this makes
  View.merge_path looks like a merge between branches. All the
  view operations are squashed in a unique commit.
* Better graphs, where we only show the commit history (the
  full graph is still available using `--full` on the
  command-lineor or `?full=1` on the web interface)
* By default, do not call `dot` when dumping a graph on the
  command-line. `dot` does not like big graphs, but that's
  still useful to have the `.dot` file to analyze it.

## 0.8.2 (2014-06-11)
* Support backend specific protocols for push/pull
* The Irmin Git backend can now sync with remote Git repositories
* Simplify the organisation of the libraries: irmin, irmin.backend,
  irmin.server and irmin.unix (check how the example are compiled)
* Small refactoring to ease the use of the API. Now use `open Irmin_unix`
  at the top of your file and use less functor in your code (again,
  check the examples)

## 0.8.1 (2014-06-02)
* Fix the behavior of `IrminMemory.Make` to return an hanlder to a
  shared datastore instead of creating a fresh one. Add
  `IrminMemory.Fresh` to return a fresh in-memory datastore maker.
* The HTTP server now outputs some nice graph (using dagre-d3). Don't
  expect to display very large graphs
* More friendly tag names in the Git backend (no need to prefix
  everything by `refs/heads/` anymore)
* Partial support for recursive stores (WIP)

## 0.8.0 (2014-05-27)
* Spring clean-ups in the API. Separation in IrminBranch for
  fork/join operations, IrminSnapshot for snapshot/revert
  operations and IrminDump for import/export operations.
  The later two implementation can be derived automaticaly
  from a base IrminBranch implementation. The update and merge
  operations are supported on each backend
* IrminGit does not depend on unix anymore and can thus be
  compile to javascript or xen with mirage
* No need to have bin_io converter for contents anymore
* No need to have JSON converter for contents anymore
* No more IrminDispatch
* Add an optional branch argument to Irmin.create to use
  an already existing branch
* Fix order of arguments in Irmin.merge

## 0.7.0 (2014-05-02)
* Feature: support for in-memory transactions. They are built
  on top of views.
* Feature: add support for views: these are temporary stores with
  lazy reads + in-memory writes; they can be used to convert back
  and forth an OCaml value into a store, or to have a fast stagging
  area without the need to commit every operation to the store.
* Support custom messages in commit messages
* Improve the IrminMerge API
* Backend: add a 'dispatch' backend for combining multiple backends
  into one. This can be used to have a P2P store where there is
  well-defined mapping between keys and host (as a DHT).
* Fix: limit the number of simulteanous open files in the Git and
  the file-system backend
* Speed-up the in-memory store
* Speed-up the import/export codepath
* Speed-up the reads
* Speed-up IrminValue.Mux
* Deps: use ocaml-sha instead of cryptokit

## 0.6.0 (2014-04-12)
* Support for user-defined contents (with custom merge operators)
* Support for merge operations
* Rename `IrminTree` to `IrminNode` to reflect the fact that we
  can support arbitrary immutable graphs (it's better if they are
  DAGs but that's not mandatory)
* Rename `IrminBlob` to `IrminContents` to reflect the fact that
  we also support structured contents (as JSON objects)
* Support for linking the library without linking to camlp4 as well (#23)

## 0.5.1 (2014-03-02)
* Port to use Cohttp 0.10.0 interface.

## 0.5.0
* More consistent support for notifications. `irmin watch` works
  now for all backends.
* Support for different blob formats on the command-line
* Support for JSON blobs
* More flexible `irmin fetch` command: we can now choose the backend to
  import the data in
* Fix import of Git objects when the blobs were not imported first
* Support non-UTF8 strings as path name and blob contents (for all
  backends, including the JSON one)
* Speed-up the `slow` tests execution time
* Improve the output graph when objects of different kinds might have
  the same SHA1

## 0.4 (2014-01-21)
* The command-line tool now looks in the environment for the variable
  `IRMIN` to configure its default backend
* Add a Git backend
* Add Travis CI scripts to the repo
* Use `Lwt_bytes` and `Lwt_unix` instead of the custom-made `IrminChannel`
* Use `bin_prot` instead of a custom binary protocol
* Major refactoring: `Value` is now `Blob`, `Revision` is now `Commit`
   and `Tag` becomes `Reference` (rational: consistency with Git names)
* Use `core_kernel` instead of building a custom `Identiable.S`
* Use `dolog` instead of a custom log library
* Use `mstruct` (mutable buffers on top of `cstruct`) which is now
  released independently

## 0.3 (2013-12-13)
* Fix a fd leak in the filesystem bakend
* Functorize the CRUD interface over the HTTP client implementation
* Use oasis to build the project
* Use the now released separately `ezjsonm` and `alcotest` libraries

## 0.2 (2013-11-23)
* Fix the HTTP server responses
* More high-level tests
* Add unit-tests for the client CRUD interfaces (over memory and/or filesystem)
* Fix issues with the Tree API
* Implement a relatively efficent Import/Export scheme (#3)
* For more safety, the marshalled values are now typed in the binary protocol
* Add functions to dump the contents of the store as a Graphviz graph
* Polish the CLI which now looks usable enough
* Optimize the CRUD backend by executing high-level API functions on the server
* Improve and make the CLI easier to use
* Implement clone/pull/push/snapshot/revert in the CLI

## 0.1 (2013-10-30)
* Use an HTTP server as a front-end
* Initial support for in-memory and filesystem backends
* Simple signature for backends
* Binary protocol for storing values and metadata and for future network exchange
