### 1.0.0 (unreleased)

Major API changes:

- It is now simpler to define mergeable contents, using new
  combinators to describe data-types (see `Type`).

- The mutable views have been replaced by immutable trees, and made
  first-class citizen in the API (see available `S.Tree`).
  Transactions now only ensure snapshot isolation instead of full
  serialisability.

- the backend and user-facing API are now totally independant (instead
  of being half-included in each other in `irmin.0.*`), so that
  backends have to implement the minimum set of functions to be
  Irmin-compatible, and users can have many convenient high-level
  functions when using the Irmin API. The backends implement
  `AO` and `RW`, the frontend provides `S`.

The package is also now split into 5 opam packages: `irmin`, irmin-git`,
`irmin-http`, `irmin-unix` and `irmin-mirage` with similarly named
`ocamlfind` libraries.

More detailled changes:

* use result type everywhere (#397, @samoht)
* use `Fmt` everywhere (#397, @samoht)
* rename `create` functions into `v` (#397, @samoht)

**irmin**

* [info] rename `Task` into `Info` to denote commit info (#397, @samoht)
* [info] remove `Task.uid` (#397, @samoht)
* [info] Commit messages are now plain strings (instead of a lists of
  strings): change `Task.messages` into `Info.message`, take a string
  instead of a list of strings as parameter and remove `Task.add`
  (#397, @samoht)
* [info] change `Info.f` to only takes `unit` as argument. Previously
  it was taken an `'a` which was used by the update functions. The
  update functions now take a full `Info.f` function as parameter,
  which should be less confusing (#397, @samoht)

* [merge] replace the dependency to `mirage-tc` by a new internal module
  `Type` using type-based combinators. This makes defining new mergeable
  data-types much easier, especially records and variants (#397, @samoht)
* [merge] change [Merge.t] to be an abstract type (#397, @samoht)
* [merge] add [Merge.f] to transform a [Merge.t] value into a merge function
  (#397, @samoht)
* [merge] add base merge combinators: `Merge.unit`, `Merge.bool`,
  `Merge.char`, `Merge.int`, `Merge.int32`, `Merge.int64`,
  `Merge.float` (#397, @samoht)
* [merge] simplify the type of `Merge.option`, `Merge.pair`, Merge.triple` and
  `Merge.alist` (#397, @samoht)
* [merge] simplify and rename `Merge.MSet` into `Merge.MultiSet` (#397, @samoht)
* [merge] simplify and rename `Merge.set` into `Merge.Set` (#397, @samoht)
* [merge] rename `Merge.OP` into `Merge.Infix` and rename operators to
  avoid name-clashing with other monads (#397, @samoht)
* [merge] remove the `path` argument from the merge functions (#397, @samoht)
* [merge] remove the need to defined a `Path` submodule in `Contents.S`
  (#397, @samoht)
* [merge] add a (very simple at the moment) `Diff` module (#397, @samoht)

* [api] read operations do not take a task parameter anymore (#397, @samoht)
* [api] write operations not take a full commit info instead of a confusing
  `'a` parameter (#397, @samoht)
* [api] rename [Ref] into [Branch] (#397, @samoht)
* [api] replace `S.read` by `S.find` (#397, @samoht)
* [api] replace `S.read_exn` by `S.get` (#397, @samoht)
* [api] add `S.kind` to check the kind of store entries (files, directories)
  (#397, @samoht)
* [api] remove the `View` functor, replaced by first-class support for
  immutable trees `S.Tree` (#397, @samoht)
* [api] add `S.find_tree` to find immutable subtrees (#397, @samoht)
* [api] add `S.find_all` to find contents and metadat (#397, @samoht)
* [api] change `S.mem` to only check for contents, not subtree (#397, @samoht)
* [api] add `S.mem_tree` to check for subtrees (similar behavior to
  `S.mem` in `irmin.0.*`) (#397, @samoht)
* [api] add `S.with_tree` for atomic update of subtrees. This
  operation replaces `with_hrw_view`, but a weaker consistency
  guarantee: instead of providing full seriasilabilty, `S.with_tree`
  provides snapshot isolation, which is consistent enough for most of
  the users. (#397, @samoht)
* [api] rename `S.update` into `S.set` and ensure that the operation is
  atomic by using a combination of test-and-set and optimistic concurrency
  control. (#397, @samoht)
* [api] change `S.remove` to ensure the operation is atomtic.
* [api] add `S.status` to mimick `git status`. (#397, @samoht)
* [api] remove all the `_id` suffixes. (#397, @samoht)
* [api] add `S.merge_with_commit` and `S.merge_with_branch` (#397, @samoht)
* [api] more precise return type for `S.Head.fast_forward` (#401, @samoht)
* [api] add `S.Commit`, `S.Branch` (#401, @samoht)


* [backend] replace `RO.read` by `RO.find` (#397, @samoht)
* [backend] no more `RO.read_exn` (#397, @samoht)
* [backend] no more `RO.iter`, replaced by `RW.list` (#397, @samoht)
* [backend] replace `RW.update` by `RW.set` (#397, @samoht)
* [backend] rename `RW.compare_and_set` into `RW.test_and_set` (#397, @samoht)
* [backend] new `RW.watch`, `RW.watch_key` and `RW.unwatch` functions
  to set-up low-level notifications (#397, @samoht)

**irmin-git**

- Adapt to `git.0.10.0` (#397, @samoht)
- Remove the `LOCK` modules (#397, @samoht)
- Change `Irmin_git.config` to require a `root` parameter (it was
  optional in `irmin.0.*`) (#397, @samoht)
- Rename `S.Internals` into `S.Git` (#397, @samoht)
- Rename `S.Internals.commit_of_id` into `S.Git.git_commit` (#397, @samoht)
- Add `S.Git.of_repo` to convert an Irmin repo into a Git repo (#397, @samoht)
- Add `S.Git.to_repo` to convert a Git repo into an Irmin repo (#397, @samoht)
- Expose `S.Git_mem.clear` and `S.Git_mem.clear_all` for in-memory Git
  backends (#397, @samoht)

**irmin-mirage**

- Adapt to Mirage3 (@hannesm, @yomimono, @samoht)
- Change `Irmin_git.config` to require a `root` parameter (it was
  optional in `irmin.0.*`) (#397, @samoht)

**irmin-http**

- Remove the high-level HTTP API (#397, @samoht)
- Rewrite the low-level (backend) API using `ocaml-webmachine` (#397, @samoht)

**irmin-unix**

- Rename `Irmin_unix.task` into `Irmin_unix.info` (#397, @samoht)
- Remove `LOCK`  (#397, @samoht)

### 0.12.0 (2016-11-17)

* Depends on irmin-watcher 0.2.0 to use portable file-system watches
  (fsevents on OSX or inotify on Linux) to replace the slow and CPU
  intensive file-system polling that was the default (#380, @samoht)
* Do not use `Lwt_unix.fork` in the tests anymore (#383, @samoht)
* Switch from Stringext to Astring (#382, @samoht)
* Fix regression in the tests for using Git over HTTP (#376, @samoht)
* Catch top-level exceptions in watch callbacks (#375, @samoht)
* Fix merge of assoc list with no common ancestor (#374, @samoht)
* Improve documentation for Git bare repositories (#363, @kayceesrk)
* New functor `Make_with_metadata` to customize the type of the
  nodes metadata (#364, @samoht)
* Remove mentions of private modules from the public interface
  (#364, @samoht)

### 0.11.1 (2016-06-14)

* Fix compilation of examples (#359, @samoht)

### 0.11.0 (2016-05-04)

* Use Logs (#342, @talex5)
* Improve non-unix portablity of `Irmin_fs` (#345, @samoht)
* Change the signature of `Store.iter` to defer opening the
  file only when needed. This was causing a file-descriptor
  early exhaustion on Windows (#345, @samoht)
* Fix paths for references on Windows (#345, @samoht)
* Port to `ocaml-git` 1.8.0
* Rather large API change in `Irmin.Private.Contents.Store`
  and `Irmin.Private.Commit.Store` to make it easier to
  build new and efficient Irmin backends. (#346, @samoht)
* Fix performance problem in the computation of LCAs (#351, @talex5)
* Fix sort order for Git trees (#352, @talex5)

### 0.10.1 (2015-11-26)

* Support for launchd: the `--address` argument of the CLI now
  supports a URI `launchd://<name>` where `<name>` corresponds
  to the section in the property list file (#321, by @djs55)
* Expose `/watch-rec` in the REST API (#326, by @samoht)
* Expose Store.Key = Contents.Path in Irmin.Maker. Otherwise,
  the type of steps is abstract. (#327, by @talex5)

### 0.10.0 (2015-10-14)

* Fix the `Irmin_mem` backend to work when equal keys might be not
  structurally equal (`Pervasives.(=)` is evil)
* Fix `Hash.SHA1.equal` to always return true when the underlying
  bigarrays are equals. Before that, this was only the case when
  the whole `Cstruct.t` where identical: ie. same bigarray but also
  same offset in the `Cstruct.t` value, which is obviously not
  always the case. Apply the same fix to `Hash.SHA1.compare` and
  `Hash.SHA1.hash`.
* Renamed "tag" to "branch" in the API, as "tag" is confusing for Git
  users. `BC.tag` is now `BC.name` and `BC.branch` is now `BC.head_ref`.
  The various "Tag" modules are now called "Ref" ("Branch" would be
  confusing here since they only store references to commits, not the
  branch contents).
  Note: The remote HTTP protocol still uses "tag".
* Remove `Irmin_http_server.listen`. Instead, return the Cohttp
  configuration for the server and let the user perform the listen. The
  resulting API is simpler (removes `timeout` and `uri` parameters),
  more flexible, and easier to use from Mirage.
* Remove `Irmin.task` from API of internal stores (commit, node, etc).
  Tasks are now passed explicitly to operations that need them, so it
  is now explicit which operations create commits. For example, the
  API now makes it clear that `lcas` doesn't change anything, while
  `lca` requires a task because it may create commits.
  Apart from simplifying the code, this change also makes it possible to
  create the internal stores once, not once per commit message.
  Note: this does not affect the main BC API, so most users will see no
  difference.
* Remove `Irmin.Basic`. This was a functor that took a functor for making
  stores and returned a functor for making stores with strings for
  branch names and SHA1 for the hash. It's easier to write the
  application out in full than to explain to people what it does.
  This change also makes it possible for back-ends to provide extra
  operations in a type-safe way. In particular, `Irmin_git.Internals`
  has moved inside the store type and the runtime check that it is only
  used with the correct store type is now enforced at compile time
  instead.
* Removed `AO.config`. It was only used by the removed `Git.Internals` hack.
* Moved `AO.create` to `AO_MAKER`.
* Remove dummy functions that are no longer needed with the new API:
  - `View.task` is gone (it never did anything).
  - `View.create` is gone (it ignored both its arguments and called `View.empty`).
  - `Ir_node.Graph.Store.create` (unused, but previously required by `AO`).
  - `Ir_commit.History.Store.create` (same).
* Removed the unused-and-not-exported `Ir_bc.Make` and `Ir_bc.MAKER`
  features.
* Combine `Ir_bc.STORE_EXT` and `Ir_s.STORE`. `Ir_s` was the only
  consumer of the `Ir_bc.STORE_EXT` interface, and all it did was repack
  the values to match its own interface. Now, `Ir_bc` exports the final
  public API directly, which simplifies the code.
* Moved module types into `ir_s.mli` and removed `ir_s.ml`.
  Before, all module types were duplicated in the .ml and .mli files.
* `BC` stores now contain a `Repo` module. A `Repo.t` represents a
  repository as a whole, rather than any particular branch. Operations
  which do not look at the current branch have been moved to this
  module. They are: `branches`, `remove_branch`, `heads`,
  `watch_branches`, `import`, `export`, and `task_of_head`.
  When updating old code, you can use `BC.repo t` to get a `Repo.t`
  from a branch.
  Note that `heads` previously ensured that the current branch's head was
  included in the returned set (which made a difference for anonymous
  branches). This feature has been removed. In the future, the plan is
  to use OCaml's GC to track which anonymous branches are still being
  used and return all of them.
* The internal stores (commit, node, etc) used to implement a full `BC`
  store are now created by the back-ends, not by `Ir_bc`. This allows
  back-ends to use their own APIs for this. In particular, back-ends
  can now share resources (such as a database connection) between
  stores. Internal stores no longer need to deal with `config` values
  at all.
* `Sync.create` now takes a `Repo.t`, not a `config`, allowing
  `Repo.config` to be removed and allowing sharing of the back-end's
  internal state with the sync code. For example, the Git back-end
  no longer needs to create a new Git store object for sync.
* Change `type head` to `type commit_id`. `head` was confusing because
  it applied to all commits, not just branch heads. Putting `id` in the
  name makes it clear that this is just data and (for example) holding
  an ID will not prevent the corresponding commit from being GC'd (once
  we have GC). `of_head` is now `of_commit_id`, `task_of_head` is now
  `task_of_commit_id`, `Internals.commit_of_head` is now
  `Internals.commit_of_id` and `BC.Head` is now `BC.Hash`.

### 0.9.10 (2015-10-01)

* Expose the Git compression level (#104, #298 by @samoht)
* Add an optional `config` argument to all the backend's config
  functions. This allow the backends to composed more easily. (initial
  patch by @nasrallahmounir, integration by @samoht)
* Add signatures for immutable link store, to store links between
  keys: `Irmin.LINK`  and `Irmin.LINK_MAKER`. Add `Irmin_mem.Link` and
  `Irmin_fs.Link` which implement `Irmin.LINK_MAKER` in these backends
  (initial patch by @nasrallahmounir, integration by @samoht)
* Add signatures for raw values (ie. whose values are of type
  `Cstruct.t`): `Irmin.RAW` and raw store maker: `Irmin.AO_MAKER_RAW`
  (initial patch by @nasrallahmounir, integration by @samoht)
* Expose `Irmin.Hash.digest_size` (initial patch by @nasrallahmounir,
  integration by @samoht)
* Expose `/view` to the REST API (#292, by @samoht)
* Expose `Irmin.Private.merge_node` (#292 by @samoht)
* Change the JSON stream API, which requres ezjsonm.0.4.2. (#266, #269,
  #273 by @samoht)
* Fix a race when a lot of processes are trying to add a watch at the
  same time. (#270, #271, by @samoht)
* Expose `Irmin_git.Irmin_value_store` functor. This provides the
  Irmin Contents/Node/Commit APIs on top of a Git-type store. This is
  useful for backends that want to store data using the Git object
  format, to be able to sync with Git, but without using Git's
  filesystem layout and locking. (#268 by @talex5)
* Remove the first-class module API. It's confusing to duplicate the API
  (#293, by @talex5)

### 0.9.9 (2015-08-14)

* Allow raw bodies in queries and responses for the REST API. This is
  controlled by the `Content-type` field set by the client:
  by default, we still use JSON (or use `application/json`) but using
  `application/octet-stream` will avoid having to hex-encode large
  binary blobs to make them JSON-compatible. This feature is still
  experimental (especially when using Git on the server) (#255)
* Adapt to `ocaml-git.1.7.1` (which works with `lwt.2.5.0`)
* Expose `Store.config` for all the stores (`AO`, `RW`, etc.)
* Expose `Irmin_git.Internals` to be able to get back the
  Git commit objects from an `head` value (#245, #241)
* Expose `Irmin.Private.remove_node`
* Remove the special `__root__` filename in Irmin stores and in views
  (#233)
  - This fixes `View.update_path` when the view contains a value at its
    root. Now the updated path contains a the value stored at the root
    of the view.
  - Writing a value to the root of a store is now an error
  - Reading a value at the root of a store always return `None`
* Make the HTTP backend re-raise the `Invalid_argument` and `Failure`
  exceptions that were raised by the server.

### 0.9.8 (2015-07-17)

* Fix wrong interaction of in-memory views and temporary branches in the store
  (#237)
* Fix `Irmin.update_tag` for HTTP clients
* Initial MirageOS support. Expose `Mirage_irmin.KV_RO` to surface an
  Irmin store as a read-only key/value store implementing `V1_LWT.KV_RO
  (#107)
* Expose `Irmin_git.Memory_ext. This allows the Git memory backend to be
  configured with a non-empty conduit context.
* Expose `Irmin.SYNC`
* Transmit client tasks to the HTTP server on DELETE too (#227, @dsheets)
* Do note expose private types in the public interface (#234, @koleini)
* Fix missing zero padding for date pretty-printing (#228, @dsheets)
* Update the tests to use `ocaml-git.1.6.0`
* Improve the style of the HTTP commit graph.
* Constraint the string tags to contain only alpha-numeric characters
  and few mores (`-`, `_`, '.' and `/`) (#186)
* Fix a race condition in `Irmin.clone`. (#221)
* Escpate double quotes in the output of commit messages to workaround
  HTML display issues. (#222)

### 0.9.7 (2015-07-06)

* Add a version check for HTTP client and server. The client might add the
  version in the HTTP headers using the `X-IrminVersion` header - the server
  might decide to enfore the version check or not. The server always reply
  with its version in the JSON reply, using a `version` field. The client
  might use that information to bail out nicely instead of failing because
  of some random unmarshalling errors due to API changes (#167)
* Fix a regression in 0.9.5 and 0.9.6 when inserting new child in Git trees.
  This could cause a tree to have duplicate childs having the same names,
  which would confuse the merge functions, make `git fsck` and `git gc`
  complain a lot (with good reasons) and do some fency things with git
  index. The regression has been introduced while trying to fix #190 (the fix
  is in #229)

#### 0.9.6 (2015-07-03)

* Fix the datamodel: it is not possible to store data in intermediate nodes
  anymore (#209)
* Fix serialization of slices (#204)
* Do not fail silently when the synchronisation fails (#202)
* Fix a race in the HTTP backend between adding a watch and updating the store.
  In some cases, the watch callback wasn't able to see the first few updates
  (#198)
* Fix a race for all the on-disk backends between adding a watch and updating
  the store. This is fixed by making `Irmin.Private.Watch.listen_dir` and
  `Irmin.Private.Watch.set_listen_dir_hook` synchronous.
* Update the tests to use `alcotest >= 0.4`. This removes the dependency towards
  `OUnit` and `nocrypto` for the tests.
* Make the file-locking code a bit more robust

### 0.9.5 (2015-06-11)

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

### 0.9.4 (2015-03-16)

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

### 0.9.3 (2015-01-04)

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

### 0.9.2 (2015-01-19)

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

### 0.9.1 (2014-12-26)

* Port to Cohttp 0.14.0+ HTTP interface (#102)

### 0.9.0 (2014-12-20)

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

### 0.8.3 (2014-06-25)

* Views now keep track of their parent commits - this makes
  View.merge_path looks like a merge between branches. All the
  view operations are squashed in a unique commit.
* Better graphs, where we only show the commit history (the
  full graph is still available using `--full` on the
  command-lineor or `?full=1` on the web interface)
* By default, do not call `dot` when dumping a graph on the
  command-line. `dot` does not like big graphs, but that's
  still useful to have the `.dot` file to analyze it.

### 0.8.2 (2014-06-11)

* Support backend specific protocols for push/pull
* The Irmin Git backend can now sync with remote Git repositories
* Simplify the organisation of the libraries: irmin, irmin.backend,
  irmin.server and irmin.unix (check how the example are compiled)
* Small refactoring to ease the use of the API. Now use `open Irmin_unix`
  at the top of your file and use less functor in your code (again,
  check the examples)

### 0.8.1 (2014-06-02)

* Fix the behavior of `IrminMemory.Make` to return an hanlder to a
  shared datastore instead of creating a fresh one. Add
  `IrminMemory.Fresh` to return a fresh in-memory datastore maker.
* The HTTP server now outputs some nice graph (using dagre-d3). Don't
  expect to display very large graphs
* More friendly tag names in the Git backend (no need to prefix
  everything by `refs/heads/` anymore)
* Partial support for recursive stores (WIP)

### 0.8.0 (2014-05-27)

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

### 0.7.0 (2014-05-02)

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

### 0.6.0 (2014-04-12)

* Support for user-defined contents (with custom merge operators)
* Support for merge operations
* Rename `IrminTree` to `IrminNode` to reflect the fact that we
  can support arbitrary immutable graphs (it's better if they are
  DAGs but that's not mandatory)
* Rename `IrminBlob` to `IrminContents` to reflect the fact that
  we also support structured contents (as JSON objects)
* Support for linking the library without linking to camlp4 as well (#23)

### 0.5.1 (2014-03-02)

* Port to use Cohttp 0.10.0 interface.

### 0.5.0 (2014-02-21)

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

### 0.4 (2014-01-21)

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

### 0.3 (2013-12-13)

* Fix a fd leak in the filesystem bakend
* Functorize the CRUD interface over the HTTP client implementation
* Use oasis to build the project
* Use the now released separately `ezjsonm` and `alcotest` libraries

### 0.2 (2013-11-23)

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

### 0.1 (2013-10-30)

* Use an HTTP server as a front-end
* Initial support for in-memory and filesystem backends
* Simple signature for backends
* Binary protocol for storing values and metadata and for future network exchange
