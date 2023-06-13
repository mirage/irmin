## Unreleased

### Added

- **irmin**
  - Change behavior of `Irmin.Conf.key` to disallow duplicate key names by
    default. Add `allow_duplicate` optional argument to override. (@metanivek,
    #2252)
    
- **irmin-pack**
  - Add maximum memory as an alternative configuration option, `lru_max_memory`,
    for setting LRU capacity. (@metanivek, #2254)

### Changed

- **irmin**
  - Lower bounds for `mtime` is now `2.0.0` (@patricoferris, #2166)

### Fixed

- **irmin-cli**
  - Changed `--store irf` to `--store fs` to align the CLI with what is
    published on the Irmin website (@wyn, #2243)

## 3.7.1 (2023-05-24)

### Fixed

- **irmin-pack**
  - Fix issue when migrating v2 stores to use lower layer (@metanivek, #2241)
  - Fix issue when calling GC for a commit in the lower after migration
    (@metanivek, #2242)

## 3.7.0 (2023-04-26)

### Added

- **irmin**
  - Add `Conf.pp` and `Conf.equal` to print and compare configuration values
    (#2227, @samoht)
  - Add a `clear` optional arguments to all function that adds a new commit:
    `Commit.v`, `set`, `set_tree`, `remove`, `test_and_set`,
    `test_and_set_tree`, `test_set_and_get`, `test_set_and_get_tree`, `merge`,
    `merge_tree` and `with_tree`. This new argument allows to control whether
    the tree caches are cleared up after objects are exported to disk during
    the commit. (#2225, @samoht)

- **irmin-pack**
  - Add configuration option, `lower_root`, to specify a path for archiving data
    during a GC. (#2177, @metanivek)
  - Add `is_split_allowed` to check if a store allows split. (#2175, @metanivek)
  - Add `add_volume` to allow creating new empty volume in lower layer. (#2188,
    @metanivek)
  - Add a `behaviour` function to the GC to check wether the GC will archive or
    delete data. (#2190, @Firobe)
  - Add a migration on `open_rw` to move the data to the `lower_root` if
    the configuration was enabled (#2205, @art-w)

### Changed

- **irmin**
  - Expose type equality for `Schema.Info` to avoid defining the `info` function
    multiple times when using similar stores (#2189, #2193, @samoht)
- **irmin-pack**
  - GC now changes its behaviour depending on the presence of a lower layer.
    (#2190, @Firobe)
  - Split now raises an exception if it is not allowed. It is not allowed on
    stores that do not allow GC. (#2175, @metanivek)
  - GC now supports stores imported V1/V2 stores, in presence of a lower layer
    only. (#2190, @art-w, @Firobe)
  - Upgrade on-disk format to version 5. (#2184, @metanivek)
  - Archive to lower volume does not copy orphaned commits. (#2215, @art-w)

### Fixed
- **irmin-pack**
  - Unhandled exceptions in GC worker process are now reported as a failure
    (#2163, @metanivek)
  - Fix the silent mode for the integrity checks. (#2179, @icristescu)
  - Fix file descriptor leak caused by `mmap`. (#2232, @art-w)

## 3.6.1 (2023-03-15)

### Fixed

- **irmin-pack**
  - Clear LRU when calling `reload` after a GC (#2200, @metanivek)

## 3.5.2 (2023-03-02)

### Fixed

- **irmin-pack**
  - Clear LRU when calling `reload` after a GC (#2200, @metanivek)

## 3.6.0 (2023-02-16)

### Changed

- **irmin-pack**
  - Improve GC reachability traversal to optimize memory, speed and remove
    the need for temporary files. (#2085, @art-w)

## 3.5.1 (2023-01-10)

### Fixed

- **irmin-pack**
  - Integrity check of a commit works on stores using the minimal indexing
    strategy. (#2160, @icristescu)

## 3.5.0 (2022-12-15)

### Added

- **irmin-pack**
  - Add `Irmin_pack_unix.Stats.Latest_gc` which is now the parameter of GC's
    `finished` callback (#2089, @Ngoguey42)
  - Add `Gc.oldest_live_commit` which returns the key of the commit on which the
    latest gc was called on. (#2110, @icristescu)
  - Add `split` to create a new suffix chunk. Subsequent writes will append to
    this chunk until `split` is called again. (#2118, @icristescu)
  - Add `create_one_commit_store` to create a new store from the existing one,
    containing only one commit. (#2125, @icristescu)

### Changed

- **irmin-pack**
  - Upgraded on-disk format to version 4. (#2110, @icristescu)
  - Detecting control file corruption with a checksum (#2119, @art-w)
  - Change on-disk layout of the suffix from a single file to a multiple,
    chunked file design (#2115, @metanivek)
  - Modify GC to work with new chunked suffix. See `examples/gc.ml` for a
    demonstration of how it works with the new `split` function. (#2126,
    @metanivek)

## 3.4.3 (2022-10-19)

### Fixed

- **irmin-pack**
  - Fix read-only opening flags of mapping for read-only opening of stores that
    do not have read-write rights on the files. (#2121, @Ngoguey42)

## 3.4.2 (2022-10-06)

### Added

- **irmin**
  - Add `test_set_and_get*` functions to retrieve commit associated with an update to the store (#2074, @patricoferris)

- **irmin-graphql**
  -  Expose `test_set_and_get` function as a new mutation (#2074, @patricoferris)
  -  Add `contents_hash` function to get a value's hash (#2099, @patricoferris)

- **irmin-pack**
  - Expose `Gc.cancel` to abort a running GC (#2101, @art-w)
  - Add `Irmin_pack_unix.Stats.Latest_gc` which is now the parameter of GC's
    `finished` callback (#2089, @Ngoguey42)

- **irmin-tezos-utils**
  - Add package `irmin-tezos-utils` containing a graphical tool for manual pack
    files analysis. (#1939, @clecat)

### Changed

- **irmin-pack**
  - `irmin_pack_mem` no longer exposes disk specifics functions (#2081,
  @icristescu)
  - Move unix specific details for `Pack_key` and `Pack_value` from `irmin-pack`
    to `irmin-pack.unix` (#2084, @metanivek)
  - Remove unnecessary files at `open_rw` and after a failed GC (#2095, @art-w)
  - Move the gc commit from the suffix file to the prefix. (#2102, @icristescu)

### Fixed

- **irmin-pack**
  - Fix data race in RO instances when reading control file (#2100, @Ngoguey42)
  - Fix bugs in gc related to commits that share the same tree. (#2106,
    @icristescu)
  - Fix the traverse pack files commands in the `irmin-tezos` CLI to work with
    gced stores. (#1919, @icristescu)

## 3.4.1 (2022-09-07)

### Added

- **irmin**
  - Add `Storage` module for creating custom storage layers (#2047, @metanivek)

- **irmin-pack**
  - Add `Gc.is_allowed` (#2076, @icristescu)
  - Add a `weight` parameter in the LRU implementation to bound
    memory usage (#2050, @samoht)

### Changed

- **irmin**
  - Removed `Irmin_unix.set_listen_dir_hook` (#2071, @zshipko)

### Fixed

- **irmin-pack**
  - Fix the behaviour of irmin-pack regarding hashes and keys to GCed objects.
    It used to not correctly ignore these entries, which could have resulted in
    various bugs. E.g. the impossibility to append an object that used to be
    dead and that has its hash in index. (#2070, @Ngoguey42)

## 3.4.0 (2022-08-25)

### Added

- **irmin**
  - Add `Tree.seq` to `Tree`'s public API (#1923, @metanivek)

- **irmin-fs**
  - Add unix subpackage, `irmin-fs.unix` (#1948, @metanivek)

- **irmin-git**
  - Add unix subpackage, `irmin-git.unix` (#1948, @metanivek)

- **irmin-graphql**
  - Add unix subpackage, `irmin-graphql.unix` (#1948, @metanivek)

- **irmin-http**
  - Add unix subpackage, `irmin-http.unix` (#1948, @metanivek)

- **irmin-cli**
  - Add new package for `irmin` command-line tool (#1951, @metanivek)

- **irmin-pack**
  - Add a garbage collection feature, allowing a user to discard commits older
    than a specified commit.  This feature is only enabled for stores that use
    `Indexing_strategy.minimal`. The primary API is `Store.Gc.run`,
    `Store.Gc.wait`, and `Store.Gc.is_finished`. See `examples/gc.ml` for a
    demonstration of usage.
  - Add a consistency check for the files on disk when opening store (#2004,
  @icristescu)

### Changed

- **irmin**
  - Replaced `Tree.node_fn` type with more general `Tree.folder` type to
    represent the different ways to use `Tree.fold` (#1918, @metanivek)

- **irmin-unix**
  - Removed the `irmin-unix` package. Unix backends are now subpackages of their
    relevant backend (see `irmin-fs.unix` and `irmin-git.unix`). The CLI tool is
    in `irmin-cli`. For common unix utilities, see `irmin.unix`. (#1953, @metanivek)

- **irmin-graphql**
  - Updated to `graphql.0.14.0` (#1843, @patricoferris, @zshipko)

### Fixed

- **irmin**
  - Fix a bug in Irmin.LRU.clear that disables the cache completly
    after a clear. This is not used in any production code as only
    the GC is clearing LRU so far (#1998, @samoht)

## 3.3.2 (2022-07-25)

### Fixed

- Add a consistency check for the files on disk when opening store
  (#2004, #2007, #@icristescu)
- Fix `irmin-tezos` CLI and `./tezos-node storage` to work with
  v2 inodes (#1903, @icristescu, @Ngoguey42)

## 3.3.1 (2022-06-22)

### Fixed

- **irmin-pack**
  - Fix topology irregularities on disk which may lead to post-gc crashes.
    (#1925, @Ngoguey42, @icristescu)

## 3.3.0 (2022-06-20)

### Added

- **irmin**
  - Add `Metrics` module to describe metric gathering in irmin.
    (#1817, @maiste)
  - Add `Repo.config` to access config used to create repo
    (#1886, @zshipko)

- **irmin-unix**
  - Add `--plugin` flag to load Dynlink plugins that can register new
    contents, hash and store types (#1808, @zshipko)

- **irmin-pack**
  - Add `use_fsync`, `dict_auto_flush_threshold` and
  - `suffix_auto_flush_threshold` in store configuration. (#1865, @Ngoguey42)
  - Add `no_migrate` in store configuration. (#1893, @zshipko)

### Changed

- **irmin-pack**
  - Move `Irmin_pack.Pack_store.Indexing_strategy` to
    `Irmin_pack.Indexing_strategy` and the rest of `Pack_store`
    to `Irmin_pack_unix` (#1833, @Ngoguey42)
  - Different repos opened using the same store module no longer share caches
    and file descriptors (#1852, @Ngoguey42)
  - `Snapshot.Import.close` requires a repo as additional argument (#1872,
    @icristescu)
  - Upgraded on-disk format to version 3 to support better synchronisation
    mechanism between readwrite and readonly instances. This change is *not*
    backwards-compatible with existing stores using `irmin-pack.x.x < 3.3.0`
    versions. A migration done by the readwrite instance is necessary to open
    older stores with `irmin-pack.3.3.0`. It is not forwards compatible. (#1865)
  - Rename `Store.sync` to `Store.reload` (#1900, @Ngoguey42).
  - Add `Pack_error` exception that centralizes many error cases alongside
    `RO_not_allowed` and `Unix.Unix_error` (#1899, @Ngoguey42)

### Fixed

- **irmin-pack**
  - Allow snapshot export to work on indexed root nodes (#1845, @icristescu)

- **irmin**
  - Fix Tree.export for nodes exported twice using different repos. (#1795,
    @Ngoguey42)

## 3.2.1 (2022-04-07)

- Support all version of cmdliner (#1803, @samoht)

## 3.2.0 (2022-03-28)

### Added

- **irmin-pack**
  - Add `forbid_empty_dir_persistence` in store configuration. (#1789,
    @ngoguey42)
  - Add `Store.Snapshot` to expose the inodes for tezos snapshots (#1757,
    @icristescu).

### Changed

- **irmin**
  - Add error types in the API or proof verifiers. (#1791, @icristescu)
  - Reduced the memory footprint of ``Tree.fold ~uniq:`True`` by a factor of 2.
    (#1770, @CraigFe)
  - Remove `clear` from all content addresssable stores. (#1794, @icristescu)

## 3.1.0 (2022-02-25)

### Fixed

- **irmin-pack**
  - Drop unnecessary runtime dependency on `ppx_irmin`. (#1782, @hhugo)
  - Split the unix part of irmin-pack into irmin-pack.unix (#1783, @hhugo)

- **irmin-unix**
  - Fix conflicting command line arguments for `push`, `pull`, `fetch` and
    `clone` (#1776, @zshipko)
  - Fix issues with Sync functions by provided a better default `Mimic.ctx`. A
    side-effect of this update is that the `remote` function now returns an Lwt
    promise. (#1778, @zshipko)

### Added

- **libirmin**
  - Create `libirmin` package providing a C interface to the irmin API
    (#1713, @zshipko)

### Changed

- **irmin-bench**
  - Make trace replay API public and simpler (#1781, @Ngoguey42)

## 3.0.0 (2022-02-11)

### Fixed

- **irmin**
  - Fix the implementation of comparison on `Irmin.Tree` objects to use the
    comparison defined on hashes. The previous implementation was unstable.
    (#1519, @CraigFe)
  - Default implementation for contents, nodes and commits can no longer trigger
    pre_hash collisions. This changes their hash. (#1715, @Ngoguey42,
    @icristescu)

- **irmin-pack**
  - Improve the performance of Index encode and decode operations by
    eliminating intermediate allocations. (#1577, @CraigFe)

- **irmin-unix**
  - Fix terms that can be manipulated at runtime by delaying computation
    (#1645, @zshipko)

### Added

- **irmin**
  - Add `Read_only.S` and `Read_only.Maker` module types (#1343, @samoht)
  - Append-only and content-addressable backend implementations have to
    provide `close` and `batch` functions (#1345, @samoht)
  - Atomic-write backend implementations have to provide a `close` function
    (#1345, @samoht)
  - Add a function `Store.Tree.singleton` for building trees with a single
    contents binding. (#1567, @CraigFe)
  - Add `with_handler` and `head` to `Store.Backend.Node` and
    `Store.Backend.Node_portable`to work with recursive node structures from
    irmin core. (#1712, #1746 @Ngoguey42). Forward
    port of #1692 and #1670.
  - Add `proof`, `to_proof` and `of_proof` to `Store.Backend.Node_portable`
    (#1716, @Ngoguey42). Forward port from #1583.
  - Add `hash_exn` to `Store.Backend.Node.Val` and `Store.Backend.Node_portable`
    (#TODO, @Ngoguey42) Forward ported from #1741.
  - Add a `Store.Tree.kinded_hash` function. (#1767, @Ngoguey) Forward ported
    from #1625.
  - Add `Contents.String_v2`, `Node.Generic_key.Make_v2` and
    `Commit.Generic_key.Make_v2` for backward compatibility with older stores.
    (#1715, @icristescu)

- **irmin-bench**
  - Many improvements to the actions trace replay:
      - Support for the layered store (#1293, @Ngoguey42)
      - Fix replay for the first ~650k commits (was ~13k) (#1314, @Ngoguey42)
      - Can change inode configuration prior to replay (#1326, @Ngoguey42)
      - Check hash of commits (#1328, @icristescu)
      - Fix the path flattening technique (#1357, @Ngoguey42)
      - Introduce a new actions trace that can support replaying up to ~1300k
        commits. (#1358, @Ngoguey42)
      - Improve the stats collection and stats report (#1367, #1384, #1403,
        #1404, #1416, #1429, #1438, #1501, #1616, @Ngoguey42, @maiste)
      - Enable replay in CI (#1430, @Ngoguey42)
      - Enable replay in CB (#1441, @Ngoguey42)

- **irmin-mem**
  - Add `Irmin_mem.Content_addressable` (#1369, @samoht)

- **irmin-pack**
  - Add a `stat-store` command to `irmin-fsck` to output stats on the tree
    under a specified commit (#1391, @icristescu, @Ngoguey42, @CraigFe).
  - Add new counters in `Stats` (#1570, @Ngoguey42).
  - Add an option to configure the index function and pick the relevant bits
    in a cryptographic hash by default (#1677 #1699, @samoht)
  - Verify inode depth invariants (#1711, @Ngoguey42). Forward port of #1665.

- **irmin-unix**
  - Update `irmin` CLI to raise an exception when an invalid/non-existent
    config file is specified (#1413, @zshipko)
  - Add `--commit` flag to CLI to load a store from a specific commit hash
    (#1721, @zshipko)

- **irmin-tezos**
  - Add a new package to mirror Tezos `tezos-context.encoding` library.
    That'll simplify building benchmarks and custom tools (#1579, @samoht)

### Changed

- **irmin**
  - `Irmin.Sync` is now a namespace: use `Irmin.Sync.Make(S)` instead of
    `Irmin.Sync(S)` (#1338, @samoht)
  - `Irmin.Private` is now `Irmin.Backend` (#1530, @CraigFe)
  - `Store.master` is now `Store.main`. The existing `Store.master` function is
    deprecated and will be removed in a future release. (#1564, @CraigFe)
  - `Store.Private` is now `Store.Backend` (#1530, @CraigFe)
  - `Store.Private.Sync` is now `Store.Backend.Remote` (#1338, @samoht)
  - `Irmin.Branch.S.master` is now `Irmin.Branch.S.main` (#1564, @CraigFe)
  - `Irmin.Private.{Commit,Node}` are now `Irmin.{Node,Commit}`. (#1471,
    @CraigFe)
  - All module types are now using snake-case and are not capitalized anymore.
    (#1341, @samoht)
  - Move signatures for backend stores into their own modules. All the
    `X_STORE` sigs have moved to `X.S`:
      - `APPEND_ONLY_STORE` is now `Append_only.S`
      - `CONTENT_ADDRESSABLE_STORE` is now `Content_addressable.S`
      - `ATOMIC_WRITE_STORE` is now `Irmin.Atomic_write.S`
    And all the `X_STORE_MAKER` have moved to `X.Maker`:
      - `APPEND_ONLY_STORE_MAKER` is now `Append_only.Maker`
      - `CONTENT_ADDRESSABLE_STORE_MAKER` is now `Content_addressable.Maker`
      - `ATOMIC_WRITE_STORE_MAKER` is now `Atomic_write.Maker`
    This gives some space to move convenient functors closer to where they
    belong:
      - `Content_addressable` is now `Content_addressable.Make`
      - New `Content_adddressable.Check_closed` and `Atomic_write.Check_closed`
    (#1342, @samoht)
  - Rename `Irmin.Make` into `Irmin.Maker` ; stage its result to return
    `Make` functor once provided with a content-addressable and an
    atomic-writes stores (#1369, @samoht)
  - Rename `Irmin.Make_ext` into `Irmin.Maker_ext` ; stage its result to
    return  `Make` functor once provided with a content-addressable and an
    atomic-writes stores, as well as node and commit makers (#1369, @samoht)
  - Require at least `lwt.5.3.0` to use `Lwt.Syntax` in the codebase
    (#1401, @samoht)
  - `Info` implementations are not part of store: use `S.Info.v`
    instead of `Irmin.Info.v` (#1400, @samoht)
  - Rename `Commit.V1` to `Commit.V1.Make`. This functor now takes separate
    hash and key implementations as arguments. (#1431 #1634, @CraigFe
    @icristescu)
  - Introduce a `Schema` module to hold all the types that users can
    define in an Irmin store. Use this as a parameter to every `Maker`
    functor. This is a large change which touches all the backends.
    (#1470, @samoht, @CraigFe)
  - Add `Irmin.Backend.Conf.Schema` for grouping configuration keys. Now
    `Irmin.Backend.Conf.key` takes an additional `~spec` parameter.
    (#1492, @zshipko)
  - `Tree.empty` and `Node.empty` now both take a unit argument. (#1566 #1629,
    @CraigFe)
  - Rename `key` type to `path` and `Key` module to `Path` when it is in a path
    context in `Tree` and `Store`. (#1569, @maiste)
  - Move `Node.default` metadata default values into a `Node.Metadata.default`
    to give room for other metadata values (#1611, @samoht)

  - Add support for non-content-addressed ("generic key") backend stores. This
    allows Irmin to work with backends in which not all values are addressed by
    their hash. In particular, this includes:
    - New functions: `Store.{Commit,Contents,Tree}.of_key`.
    - Adds `Irmin.{Node,Commit}.Generic_key` modules.
    - Adds new types that must be provided by backends: `Node.Portable` and
      `Commit.Portable`.
    - Adds a new type of backend store: `Irmin.Indexable.S`.
    (#1510 #1647, @CraigFe)
  - Cache hits in several `Tree` functions are more frequent than before.
    (#1724, @Ngoguey42, @CraigFe)
  - Add a new `Pruned_hash` tag to the error case of several `Store.Tree`
    functions (#1744 @Ngoguey42). Forward ported from #1583.

- **irmin-containers**
  - Removed `Irmin_containers.Store_maker`; this is now equivalent to
    `Irmin.Content_addressable.S` (#1369, @samoht)
  - Renamed `Irmin_containers.CAS_maker` to
    `Irmin_containers.Content_addressable` (#1369, @samoht)

- **irmin-fs**
  - Renamed `Irmin_fs.Make` into `Irmin_fs.Maker` (#1369, @samoht)
  - Renamed `Irmin_fs.Make_ext` into `Irmin_fs.Maker_ext` (#1369, @samoht)

- **irmin-git**
  - All of the configuration keys have moved into their own namespace:
    - `Irmin_git.root` is now `Irmin_git.Conf.root`
    - `Irmin_git.head` is now `Irmin_git.Conf.head`
    - `Irmin_git.bare` is now `Irmin_git.Conf.bare`
    - `Irmin_git.level` is now `Irmin_git.Conf.level`
    - `Irmin_git.buffers` is now `Irmin_git.Conf.buffers`
    - `Irmin_git.dot_git` is now `Irmin_git.Conf.dot_git`
   (#1347, @samoht)
  - Renamed `Irmin_git.Make` into `Irmin_git.Maker` (#1369, @samoht)
  -  Require at least `git.3.7.0` in the codebase (#1632, @dinosaure)

- **irmin-graphql**:
  - Changed the name of the default branch node from `master` to `main` in the
    GraphQL API. (#1564, @CraigFe)
  - Updated to be compatible with generic keys.
    - The `Key` type is now called `Path` to match the new name in `irmin`
    - All `key` fields and parameters have been renamed to `path`
    (#1618, @zshipko)

- **irmin-mirage**
  - Renamed `Irmin_mirage_git.Make` into `Irmin_mirage_git.Maker`
    (#1369, @samoht)

- **irmin-pack**
  - Changed the implementation of backend store keys to use direct pointers to
    store contents (by offset in the pack file) when possible, rather than
    querying the index on each lookup. (#1659, @CraigFe @ngoguey42 @icristescu)
  - The `Irmin_pack.Maker` module type now no longer takes a `Conf` argument.
    (#1641, @CraigFe)
  - The backend configuration type `Conf.S` requires a new parameter
    `contents_length_header` that (optionally) further specifies the encoding
    format used for commits in order to improve performance. (#1644, @CraigFe)
  - Upgraded on-disk format of pack files to support more efficient lookups and
    reduce indexing overhead.  This change is fully backwards-compatible with
    existing stores using `irmin-pack.2.x` versions, but not
    forwards compatible. (#1649 #1655, @CraigFe @Ngoguey42)
  - Added support for user-specified indexing strategies. The default strategy
    is to index all objects appended to the pack file (as before), but users may
    now choose to index fewer objects in order to improve the write performance
    of the store, at the cost of introducing potential duplicate values to the
    pack file. (#1664, #1761, @CraigFe, @maiste)

- **irmin-unix**
  - Clean up command line interface. Allow config file to be specified when
    using `Irmin_unix.Resolver.load_config` and make command line options
    take precedence over config options.
    (#1464, #1543, #1607 @zshipko)
  - `Irmin_unix.Resolver.destruct` has been removed (and partially replaced by
    `Resolver.spec`). (#1603, @CraigFe)
  - Update `irmin` CLI to support empty path in `list` subcommand.
    (#1575, @maiste)
  - Add new commands to CLI: `branches` for listing available branches and
    `log` which is similar to `git log` (#1609, #1727, @zshipko)
  - Update `irmin watch` to take parameters to specify a command that should
    be executed when there are new changes (#1608, @zshipko)

### Removed

- **irmin-pack**
  - Removed the `irmin-pack.layered` library. Support for the layered store
    will be restored on a future release of `irmin-pack`. (#1651, @CraigFe)
  - Removed support for the `clear` operation in `irmin-pack`. This operation
    is incompatible with performance optimisations made in this release.
    (#1655, @CraigFe)

- **irmin-layers**
  - This experimental package has been removed.

## 2.10.2 (2022-02-02)

### Fixed

- **irmin**
  - Fixed a bug causing stream proof extender nodes to have their segments be
    returned in reverse order (i.e. bottom to top, rather then top-down).
    (#1742, @CraigFe)

  - Fixed a bug that allowed the creation of overly-large stable inodes via
    stream proofs. (#1741, @Ngoguey42)

### Added

- **irmin**
  - Add `Store.Private.Node.Val.hash_exn` (#1741, @Ngoguey42)

## 2.10.1 (2022-01-20)

### Fixed

- **irmin**
  - Fix bug introduced in #1683 which causes `Tree.seq` and `Tree.list` to
    produce pruned children (#1720, @Ngoguey42)

## 2.10.0 (2022-01-07)

### Fixed

- **irmin**
  - Conversion between proofs and trees are now done in CPS (#1624, @samoht)
  - Better support for s390x to workaround https://github.com/ocaml/ocaml/issues/10857
    (#1694, @icristescu)

- **irmin-pack**
  - Fix proofs for large inodes by tracking side-effects reads inside the
    inode implementation (#1670, @samoht, @Ngoguey42)
  - Flush branch store without calling `Repo.close` (#1707, @zshipko)

### Added

- **irmin**
  - Add `Tree.produce_proof` and `Tree.verify_proof` to produce and verify
    proofs from complex computations. `produce_proof` and `verify_proof`
    takes a callback over tree and instead of a static list of operations
    -- this now means that the full `Tree` API can now be used in proofs,
    including sub-tree operations, folds and paginated lists
    (#1625, #1663, #1683, @samoht, @Ngoguey42)
  - Add `Tree.produce_stream` and `Tree.verify_stream` to produce and
    verify stream proofs (#1684, #1692, #1691, @samoht, @Ngoguey42, @icristescu)

- **irmin-pack**
  - Verify inode depth invariants (#1665, @samoht)

- **irmin-unix**
  - Add `tezos` store type for `irmin` command-line (#1678, @zshipko)

### Changed

- **irmin**
  - Remove `Tree.Proof.of_keys`. Use `Tree.produce_proof` instead
    (#1625, @samoht)
  - `Tree.empty` now takes a unit argument. (#1566, @CraigFe)
  - `Tree.length` now takes a tree as argument (#1676, @samoht)
  - `Tree.Proof.t` now uses a more precise datatype to encode value
    invariants (#1688, @samoht)

- **irmin-pack**
  - irmin-pack: add an option to configure the index function and pick
    the relevant bits in cryptographic a hash by default (#1677, @samoht)

- **irmin-git**
  -  Require at least `git.3.7.0` in the codebase (#1637, @dinosaure)

## 2.9.1 (2022-01-10)

### Fixed

- **irmin**
  - Better support for s390x to workaround
    https://github.com/ocaml/ocaml/issues/10857 (#1694, @icristescu)

## 2.9.0 (2021-11-15)

### Fixed

- **irmin-pack**
   - Improved the performance of Index encode and decode operations by
     eliminating intermediate allocations (up to 5% fewer minor words
     allocated) (#1577, @CraigFe)
   - Reduce the number of backend nodes built during export
     (up to 20% fewer minor words allocated) (#1553, @Ngoguey42)

### Added

- **irmin**
  - Add Merkle Proofs and expose function to convert a proof to and from a tree.
    Once converted, normal tree operations can be performed on the proof, as
    long at it access values contained in the proof.
    (#1583, @samoht, @Ngoguey42, @icristescu)

### Changed

- **irmin-pack**
  - Limit inode depth (#1596, #samoht)
  - Adapt to index 1.5.0 (#1593, @icristescu)

## 2.8.0 (2021-10-15)

### Fixed

- **irmin**
  - `Tree` operations now raise a `Dangling_hash` exception when called with a
    path that contains dangling hashes in the underlying store, rather than
    interpreting such paths as ending with empty nodes (#1477, @CraigFe)
  - Fix the pre-hashing function for big-endian architectures. (#1505,
    @Ngoguey42, @dinosaure)
  - Fix a bug in `Tree.export` where nodes could be exported before
    some of their contents, resulting in indirect hashes in irmin-pack
    (#1508, @Ngoguey42)

### Added

- **irmin**
  - `Node.seq` and `Node.of_seq` are added to avoid allocating intermediate
    lists when it is not necessary (#1508, @samoht)
  - New optional `cache` parameter to `Tree.hash`, `Tree.Contents.hash`,
    `Tree.list`, `Node.list`, `Node.seq` and `Node.find` to control the storing
    of lazily loaded data (#1526, @Ngoguey42)
  - Add `Node.clear` to clear internal caches (#1526, @Ngoguey42)
  - Added a `tree` argument to `Tree.fold` to manipulate the subtrees (#1527,
    @icristescu, @Ngoguey42)
  - Add a function `Store.Tree.pruned` for building purely in-memory tree
    objects with known hashes. (#1537, @CraigFe)
  - Added a `order` argument to specify the order of traversal in `Tree.fold`
    (#1548, @icristescu, @CraigFe)

### Changed

- **irmin**
  - `Node.v` is renamed to `Node.of_list` (#1508, @samoht)
  - Rewrite `Tree.export` in order to minimise the memory footprint.
    (#1508, @Ngoguey42)
  - Remove the ``~force:`And_clear`` case parameter from `Tree.fold`,
    ``~force:`True ~cache:false`` is the new equivalent. (#1526, @Ngoguey42)
  - `` `Tree.fold ~force:`True`` and `` `Tree.fold ~force:`False`` don't
    cache the lazily loaded data any more. Pass `~cache:true` to enable it
    again. (#1526, @Ngoguey42)
  - Do not allocate large lists in `Irmin.Tree.clear` (#1515, @samoht)

- **irmin-git**
  - Upgrade `irmin-git` to `git.3.5.0`. (#1495, @dinosaure)

## 2.7.2 (2021-07-20)

### Added

- **irmin-pack**
   - Added `integrity-check-index` command in `irmin-fsck`. (#1480, #1487
     @icristescu, @samoht)

### Changed

- **irmin-pack**
   - `reconstruct_index` is now `traverse_pack_file`, it allows for both index
     reconstruction and index checking  (#1478, @Ngoguey42)

## 2.7.1 (2021-07-02)

### Fixed

- **irmin-pack**
  - Fix termination condition of reconstruct index (#1468, @Ngoguey42)

## 2.7.0 (2021-06-22)

### Fixed

- **irmin**
  - Added `Store.Tree.length`. (#1316, @Ngoguey42)
  - Fixed fold for non-persisted, cleared trees (#1442, @samoht, @Ngoguey42)

- **irmin-layers**
  - Do not fail on double-close errors for private nodes (#1421, @samoht)

- **irmin-pack**
  - Do not clear and bump the generation for empty files (#1420, @samoht)

### Added

- **irmin-pack**
  - Added `Irmin_pack.Version.{V1,V2}` modules for convenience. (#1457,
    @CraigFe)
  - Added a `irmin-pack.mem` package (#1436, @icristescu, @craigfe)

- **irmin-graphql**
  - Added `last_modified` field to GraphQL interface (#1393, @kluvin)

### Changed

- **irmin-layers**
  - Remove `copy_in_upper` from the repo configuration. The default is now to
    copy. (#1322, @Ngoguey42)
  - Simplify the API of `freeze`. It is now possible to specify two distinct
    commit closures for the copy to lower and the copy to next upper.
    (#1322, @Ngoguey42)
  - Renamed `Irmin_layered_pack.Make` and Irmin_layers.Make` into
    `Irmin_layered_pack.Maker` and `Irmin_layers.Maker` (#1369, @samoht)
  - Renamed `Irmin_layered_pack.Make_ext` and and Irmin_layers.Make_ext` into
    into `Irmin_layered_pack.Maker_ext` and `Irmin_layers.Maker_ext`
    (#1369, @samoht)
  - Renamed `Irmin_layered_pack.Config` into `Irmin_layered_pack.Conf`
    (#1370, @samoht)
  - Readonly instances can check for an ongoing freeze (#1382, @icristescu,
    @Ngoguey42)

- **irmin-pack**
  - It is no longer possible to modify an `inode` that doesn't point to the root
    of a directory. (#1292, @Ngoguey42)
  - When configuring a store, is it no longer possible to set `entries` to a
    value larger than `stable_hash`. (#1292, @Ngoguey42)
  - Added number of objects to the output of `stat-pack` command in
    `irmin-fsck`. (#1311, @icristescu)
  - Renamed the `Version` module type into `Version.S` and `io_version` into
    `version`. The `Pack.File` and `Atomic_write` functors now take
    `Version` as their first parameter (#1352, @samoht)
  - Renamed `Irmin_pack.Make` into `Irmin_pack.V1` (#1369, @samoht)
  - Renamed `Irmin_pack.Config` into `Irmin_pack.Conf` (#1370, @samoht)
  - Renamed `Irmin_pack.Pack` into `Irmin_pack.Content_addressable` and
    `Irmin_pack.Pack.File` into `Irmin_pack.Content_addressable.Maker`
    (#1377, @samoht)
  - Moved `Irmin_pack.Store.Atomic_write` into its own module (#1378, @samoht)
  - `Checks.Reconstruct_index.run` now takes an optional `index_log_size`
    parameter for customising the interval between merges during
    reconstruction. (#1459, @CraigFe)

## 2.6.1 (2021-04-29)

This release contains 2.6.0 plus the changes described in 2.5.4.

## 2.6.0 (2021-04-13)

** Note: this release is based on 2.5.3, and does not contain 2.5.4. Use 2.6.1
for access to those changes. **

### Fixed

- **irmin**
  - Fix stack overflow exception when working with wide trees (#1313, @zshipko)

  - `Tree.of_concrete` now prunes empty subdirectories, and raises
    `Invalid_argument` if the input contains duplicate bindings. (#1385,
    @CraigFe)

- **irmin-chunk**
  - Use the pre_hash function to compute entry keys instead of
    their raw binary representation (#1308, @samoht)

### Changed

- **irmin-git**
  - Upgrade `irmin-git` with `git.3.4.0`. (#1392, @dinosaure)

## 2.5.4 (2021-04-28)

### Fixed

- **irmin-pack**
  - Revert a patch introduced in 2.3.0 which was calling `Index.try_merge`.
    This function was supposed to hint index to schedule merges after
    every commit. However, `Index.try_merge` is buggy and stacks merges
    which causes the node to block and wait for any existing merge to
    complete. We will revisit that feature in future once we fix
    `Index.try_merge` (#1409, @CraigFe)

- **irmin**
  - Fix peformance issue in `Tree.update_tree` and `Tree.add_tree` for
    large directories (#1315, @Ngoguey42)

### Added

- **irmin-pack**
  - Expose internal inode trees (#1273, @mattiasdrp, @samoht)

## 2.5.3 (2021-04-13)

### Fixed

- **irmin**
  - Fixed a bug causing equality functions derived from `Store.tree_t` to return
    false-negatives. (#1371, @CraigFe)

### Added

- **irmin**
  - Added `Store.Tree.is_empty`. (#1373, @CraigFe)

## 2.5.2 (2021-04-08)

### Fixed

- **irmin**
  - The `Tree.update_tree` and `Tree.add_tree` functions now interpret adding
    an empty subtree as a remove operation, rather than adding an empty
    directory.  (#1335, @craigfe)

- **irmin-pack**
  - Fixed a performance regression where all caches were always cleaned by
    `Store.sync` when using the V1 format (#1360, @samoht)

## 2.5.1 (2021-02-19)

- **irmin-git**
  - Use the last version of git 3.3.0. It fixes a bug about trailing LF on
    message. For Irmin users, it should not change anything (#1301, @dinosaure,
    @CraigFe)

## 2.5.0 (2021-02-16)

### Changed

- **irmin**
  - `Store.Tree.remove` is now much faster when operating on large directories.
    The commits following removals are also much faster. (#1289, @Ngoguey42)

  - Changed `Store.Tree.{of_hash, shallow}` to take kinded hashes, allowing the
    creation of unforced contents values. (#1285, @CraigFe)

  - Changed `Tree.destruct` to return _lazy_ contents values, which may be forced
    with `Tree.Contents.force`. (#1285, @CraigFe)

- **irmin-bench**
  - New features in benchmarks for tree operations (#1269, @Ngoguey42)

## 2.4.0 (2021-02-02)

### Fixed
- **irmin-pack**
  - Fix a bug in `inode` where the `remove` function could cause hashing
    instabilities. No user-facing change since this function is not being used
    yet. (#1247, @Ngoguey42, @icristescu)

- **irmin**
  - Ensure that `Tree.add_tree t k v` complexity does not depend on `v` size.
    (#1267, @samoht @Ngoguey42 and @CraigFe)

### Added

- **irmin**
  - Added a `Perms` module containing helper types for using phantom-typed
    capabilities as used by the store backends. (#1262, @CraigFe)

  - Added an `Exported_for_stores` module containing miscellaneous helper types
    for building backends. (#1262, @CraigFe)

  - Added new operations `Tree.update` and `Tree.update_tree` for efficient
    read-and-set on trees. (#1274, @CraigFe)

- **irmin-pack**:
  - Added `integrity-check-inodes` command to `irmin-fsck` for checking the
    integrity of inodes. (#1253, @icristescu, @Ngoguey42)

- **irmin-bench**
  - Added benchmarks for tree operations. (#1237, @icristescu, @Ngoguey42,
    @Craigfe)

#### Changed

- The `irmin-mem` package is now included with the `irmin` package under the
  library name `irmin.mem`. It keeps the same top-level module name of
  `Irmin_mem`. (#1276, @CraigFe)

#### Removed

- `Irmin_mem` no longer provides the layered in-memory store `Make_layered`.
  This can be constructed manually via `Irmin_layers.Make`. (#1276, @CraigFe)

## 2.3.0 (2021-01-12)

### Fixed

- **irmin-git**
  - Update `irmin` to the last version of `ocaml-git` (#1065)
    It fixes an issue on serialization/deserialization of big tree object
    (see #1001)

- **irmin-pack***
  - Fix a major bug in the LRU which was never used (#1035, @samoht)

- **irmin***
  - Improve performance of `last_modified` (#948, @pascutto)

  - Changed the pattern matching of the function `last_modified`. The case of a
    created key is now considered a modification by the function. (#1167,
    @clecat)

  - Make Tree.clear tail-recursive (#1171, @samoht)

  - Fix `Tree.fold ~force:(False f)` where results where partially skipped
    (#1174, @Ngoguey42, @samoht and @CraigFe )

  - Fix `Tree.kind`. Empty path on a tree used to return a None instead of a
    `` `Node``. (#1218, @Ngoguey42)

- **ppx_irmin**
  - Fix a bug causing certain type derivations to be incorrect due to unsound
    namespacing. (#1083, @CraigFe)

- **irmin-unix**
  - Update irmin config path to respect `XDG_CONFIG_HOME`. (#1168, @zshipko)

### Added

- **irmin-layers** (_new_):
  - Created a new package, `irmin-layers` that includes common signatures for
    layered stores. It contains a stub `Make_layers` functor (#882, @icristescu)

- **irmin-bench** (_new_):
  - Created a new package to contain benchmarks for Irmin and its various
    backends. (#1142, @CraigFe)
  - Added ability to get json output and a make target to run layers benchmark.
    (#1146, @gs0510)

- **irmin**
  - Added `Tree.Contents` module exposing operations over lazy tree contents.
    (#1022 #1241, @CraigFe @samoht)

  - Added `Type.Unboxed.{encode_bin,decode_bin,size_of}` to work with unboxed
    values (#1030, @samoht)

  - Remove the `headers` option in `Type.{encode_bin,decode_bin,size_of}`. Use
    `Type.Unboxed.<fn>` instead (#1030, @samoht)

  - `Type.v` now takes an extra mandatory `unit` argument (#1030, @samoht)

  - Added `Type.pp_dump`, which provides a way to pretty-print values with a
    syntax almost identical to native OCaml syntax, so that they can easily be
    copy-pasted into an OCaml REPL for inspection. (#1046, @liautaud)

  - Generic functions in `Irmin.Type` are now more efficient when a partial
    closure is constructed to the type representation (#1030 #1093, @samoht
    @CraigFe).  To make this even more explicit, these functions are now staged
    and `Type.{unstage,stage}` can manipulate these. The goal is to encourage
    users to write this kind of (efficent) pattern:
    ```ocaml
    let encode_bin = Type.(unstage (encode_bin ty))
    let _ = <begin loop> ... encode_bin foo ... <end loop>
    ```
  - Added a `clear` function for stores (#1071, @icristescu, @CraigFe)

  - Requires digestif>=0.9 to use digestif's default variants
    (#873, @pascutto, @samoht)

  - Added `iter_commits` and `iter_nodes` functions to traverse the commits and
    nodes graphs (#1077, @icristescu)

  - Added `Repo.iter` to traverse object graphs (#1128, @samoht)

- **irmin-pack**:
  - Added `index_throttle` option to `Irmin_pack.config`, which exposes the
    memory throttle feature of `Index` in `Irmin-Pack`. (#1049, @icristescu)

  - Added `Pack.clear` and `Dict.clear` (#1047, @icristescu, @CraigFe, @samoht)

  - Added a `migrate` function for upgrading stores with old formats (#1070,
    @icristescu, @CraigFe)

  - Added a `flush` function for a repo (#1092, @icristescu)

  - Added `Layered.Make functor, to construct layered stores from irmin-pack.
    (#882, @icristescu)

  - Added `Checks.Make which provides some offline checks for irmin-pack
    stores. (#1117, @icristescu, @CraigFe)

  - Added `reconstruct_index` to reconstruct an index from a pack file. (#1097,
    @icristescu)

  - Added `reconstruct-index` command to `irmin-fsck` for reconstructing an index from
    the command line (#1189, @zshipko)

  - Added `integrity-check` command to `irmin-fsck` for checking the integrity of
    an `irmin-pack` store (#1196, @zshipko)

- **ppx_irmin**:

  - Added support for deriving type representations for types with type
    parameters. Type `'a t` generates a representation of type
    `'a Type.t -> 'a t Type.t` (#1085, @CraigFe)

  - Added a `--lib` command-line option which has the same behaviour as the
    `lib` run-time argument (i.e. `--lib Foo` will cause `ppx_irmin` to derive
    type representations using combinators in the `Foo` module). (#1086,
    @CraigFe)

  - Added an extension point `[typ: <core-type>]` for deriving type
    representations inline. (#1087, @CraigFe)

### Changed

- **irmin**
  - Renamed the `Tree.tree` type to `Tree.t`. (#1022, @CraigFe)

  - Replaced `Tree.pp_stats` with the type representation `Tree.stats_t`. (#TODO, @CraigFe)

  - Changed the JSON encoding of special floats. `Float.nan`, `Float.infinity`
    and `Float.neg_infinity` are now encoded as `"nan"`, `"inf"` and `"-inf"`
    respectively. (#979, @liautaud)

  - The functions `Type.{v,like,map}` no longer take a `~cli` argument, and now
    take separate `~pp` and `~of_string` arguments instead. (#1103, @CraigFe)

  - The `Irmin.Type` combinators are now supplied by the `repr` package. The
    API of `Irmin.Type` is not changed. (#1106, @CraigFe)

  - `Irmin.Type` uses staging for `equal`, `short_hash` and `compare` to
    speed-up generic operations (#1130, #1131, #1132, @samoht)

  - Make `Tree.fold` more expressive and ensure it uses a bounded memory
    (#1169, @samoht)

  - Changed `list` and `Tree.list` to take optional `offset` and `length`
    arguments to help with pagination. Also return direct pointers to the
    subtrees to speed up subsequent accesses (#1241, @samoht, @zshipko,
    @CraigFe, @Ngoguey42 and @icristescu)

- **irmin-pack**:
  - `sync` has to be called by the read-only instance to synchronise with the
    files on disk. (#1008, @icristescu)

  - Renamed `sync` to `flush` for the operation that flushes to disk all buffers
    of a read-write instance. (#1008, @icristescu)

  - Changed the format of headers for the files on disk to include a generation
    number. Version 1 of irmin-pack was used for the previous format, version 2
    is used with the new format. (#1047, @icristescu, @CraigFe, @samoht)

  - Use `Repo.iter` to speed-up copies between layers (#1149, #1150 @samoht)

  - Add an option to bypass data integrity checks on reads (#1154, @samoht)

  - Add `heads` parameter to `check-self-contained` command in `Checks` (#1224, @zshipko)

- **ppx_irmin**:

  - The `[@generic ...]` attribute has been renamed to `[@repr ...]`. (#1082,
    @CraigFe)

## 2.2.0 (2020-06-26)

### Added

- **irmin**:
  - Added `Irmin.Type.empty` to represent an uninhabited type. (#961, @CraigFe)
  - Added `Store.Tree.concrete_t`. (#1003, @CraigFe)

- **irmin-containers** (_new_):
  - Created a new package, `irmin-containers`, which provides a set of simple
    mergeable datastructures implemented using Irmin. (#1014, @ani003)

- **ppx_irmin**
  - Added support for the `@nobuiltin` attribute, which can be used when
    shadowing primitive types such as `unit`. See `README_PPX` for details.
    (#993, @CraigFe)

  - Added support for a `lib` argument, which can be used to supply primitive
    type representations from modules other than `Irmin.Type`. (#994, @CraigFe)

### Changed

- **irmin**:
  - Require OCaml 4.07 (#961, @CraigFe)
  - Add sanity checks when creating `Irmin.Type` records, variants and enums
    (#956 and #966, @liautaud):
     - `Irmin.Type.{sealr,sealv,enum}` will now raise `Invalid_argument` if two
       components have the same name;
     - `Irmin.Type.{field,case0,case1}` will now raise `Invalid_argument` if
       the component name is not a valid UTF-8 string.
  - Changed the JSON encoding of options and unit to avoid ambiguous cases
    (#967, @liautaud):
    - `()` is now encoded as `{}`;
    - `None` is now encoded as `null`;
    - `Some x` is now encoded as `{"some": x}`;
    - Fields of records which have value `None` are still omitted;
    - Fields of records which have value `Some x` are still unboxed into `x`.

  - Changed pretty-printing of Irmin types to more closely resemble OCaml types.
    e.g. `pair int string` prints as `int * string`. (#997, @CraigFe)

  - The type `Irmin.S.tree` is now abstract. The previous form can be coerced
    to/from the abstract representation with the new functions
    `Irmin.S.Tree.{v,destruct}` respectively. (#990, @CraigFe)

- **irmin-mem**
  - Stores created with `KV` now expose their unit metadata type. (#995,
    @CraigFe)

### Fixed

- **irmin-graphql**
  - Fixed an issue with keys inside `get_{contents,tree}` fields having
    incorrect ordering (#989, @CraigFe)

## 2.1.0 (2020-02-01)

### Added

- **ppx_irmin** (_new_):
  - Created a new package, `ppx_irmin`, which provides a PPX deriving plugin
    for generating Irmin generics.

- **irmin-unix**:
  - Added a `--hash` parameter to the command-line interface, allowing the hash
    function to be specified. For BLAKE2b and BLAKE2s, the bit-length may be
    specified with a trailing slash, as in `--hash=blake2b/16`. The `hash`
    function may also be specified in the configuration file. (#898, @craigfe)

- **irmin**:
  - Added `Irmin.Hash.Make_BLAKE2B` and `Irmin.Hash.Make_BLAKE2S` functors for
    customizing the bit-length of these hash functions. (#898, @craigfe)
  - Added `iter` function over a closure graph (#912, @ioana)
  - Added `Type.pp_ty` for pretty-printing Irmin generics. (#926, @craigfe)
  - Added `Merge.with_conflict` for modifying the conflict error message of a
    merge function. (#926, @craigfe)

### Changed

- **irmin-pack**:
  - Changed the bit-length of serialized hashes from 60 to 30. (#897,
    @icristescu)
  - `integrity_check` can now try to repair corrupted values. (#947, @pascutto)

- **irmin-graphql**:
  - Changed default GraphQL type names to ensure uniqueness. (#944, @andreas)

## 2.0.0

### Added

- **irmin-pack** (_new_):
  - Created a new Irmin backend, `irmin-pack`, which uses a space-optimised
    on-disk format.

- **irmin-graphql** (_new_):
  - Created a new package, `irmin-graphql`, which provides a GraphQL server
    implementation that can be used with both the MirageOS and Unix backends.
    Additionally, a `graphql` command has been added to the command-line
    interface for starting `irmin-graphql` servers. (#558, @andreas, @zshipko)

  - Contents can now be queried directly using `irmin-graphql` with
    `Irmin_graphql.Server.Make_ext` and the `Irmin_graphql.Server.PRESENTER`
    interface. (#643, @andreas)

- **irmin-test** (_new_):
  - Added a new package, `irmin-test`, which allows for packages to access the
    Irmin test-suite. This package can now be used for new packages that
    implement custom backends to test their implementations against the same
    tests that the core backends are tested against. (#508, @zshipko)

- **irmin-unix**:
  - Add `Cli` module to expose some methods to simplify building command-line
    interfaces using Irmin. (#517, @zshipko)

  - Add global config file `$HOME/.irmin/config.yml` which may be overridden by
    either `$PWD/.irmin.yml` or by passing `--config <PATH>`. See `irmin help
    irmin.yml` for details. (#513, @zshipko)

- **irmin-git**:
  - Allow import/export of Git repositories using Irmin slices. (#561, @samoht)

- **irmin-http**:
  - Expose a `/trees/merge` route for server-side merge operations. (#714,
    @samoht)

- **irmin**:
  - Add `Json_value` and `Json` content types. (#516 #694, @zshipko)

  - Add optional seed parameter to the `Irmin.Type` generic hash functions.
    (#712, @samoht)

  - Add `V1` submodules in `Commit`, `Contents` and `Hash` to provide
    compatibility with 1.x serialisation formats. (#644 #666, @samoht)

  - Add `Store.last_modified` function, which provides a list of commits where
    the given key was modified last. (#617, @pascutto)

  - Add a `Content_addressable.unsafe_add` function allowing the key of the new
    value to be specified explicitly (for performance reasons). (#783, @samoht)

  - Add `save_contents` function for saving contents to the database. (#689,
    @samoht)

  - Add pretty-printers for the results of Sync operations. (#789, @craigfe)

  - `Private.Lock` now exposes a `stats` function returning the number of held
    locks. (#704, @samoht)

### Changed

- **irmin-unix**:
  - Rename `irmin read` to `irmin get` and `irmin write` to `irmin set`. (#501,
  @zshipko)

  - Switch from custom configuration format to YAML. (#504, @zshipko)

- **irmin-git**:
  - Require `ocaml-git >= 2.0`. (#545, @samoht)

  - Cleanup handling of remote stores. (#552, @samoht)

- **irmin-http**:
  - Rename `CLIENT` to `HTTP_CLIENT` and simplify the signatures necessary to
    construct HTTP clients and servers. (#701, @samoht)

- **irmin-mirage**
  - Split `irmin-mirage` into `irmin-{mirage,mirage-git,mirage-graphql}` to
    allow for more granular dependency selection. Any instances of
    `Irmin_mirage.Git` should be replaced with `Irmin_mirage_git`. (#686,
    @zshipko)

- **irmin**:
  - Update to use dune (#534, @samoht) and opam 2.0. (#583, @samoht)

  - Replace `Irmin.Contents.S0` with `Irmin.Type.S`.

  - Rename `Type.pre_digest` -> `Type.pre_hash` and `Type.hash` ->
    `Type.short_hash`. (#720, @samoht)

  - Change `Irmin.Type` to use _incremental_ hash functions (functions of type
    `'a -> (string -> unit) -> unit`) for performance reasons. (#751, @samoht)

  - Simplify the `Irmin.Type.like` constructor and add a new `Irmin.Type.map`
    with the previous behaviour.

  - Improvements to `Irmin.Type` combinators. (#550 #538 #652 #653 #655 #656
    #688, @samoht)

  - Modify `Store.set` to return a result type and create a new `Store.set_exn`
    with the previous exception-raising behaviour. (#572, @samoht)

  - Rename store module types to be more descriptive:
     - replace `Irmin.AO` with `Irmin.CONTENT_ADDRESSABLE_STORE`;
     - replace `Irmin.AO_MAKER` with `Irmin.CONTENT_ADDRESSABLE_STORE_MAKER`;
     - replace `Irmin.RW` with `Irmin.ATOMIC_WRITE_STORE`;
     - replace `Irmin.RW_MAKER` with `Irmin.ATOMIC_WRITE_STORE_MAKER`. (#601,
       @samoht)

  - Rename `export_tree` to `save_tree` (#689, @samoht) and add an option to
    conditionally clear the tree cache (#702 #725, @samoht).

  - Change hash function for `Irmin_{fs,mem,unix}.KV` to BLAKE2b rather than
    SHA1 for security reasons. (#811, @craigfe)

  - Move `Irmin.remote_uri` to `Store.remote`, for stores that support remote
    operations. (#552, @samoht)

  - Simplify the error cases of fetch/pull/push operations. (#684, @zshipko)

  - A `batch` function has been added to the backend definition to allow for
    better control over how groups of operations are processed. (#609, @samoht)

  - A `close` function has been added to allow backends to close any held
    resources (e.g. file descriptors for the `FS` backend). (#845, @samoht)

  - Simplify `Private.Node.Make` parameters to use a simpler notion of 'path' in
    terms of a list of steps. (#645, @samoht)

  - Rename `Node.update` to `Node.add`. (#713, @samoht)

### Fixed

- **irmin-unix**:
   - Fix parsing of commit hashes in `revert` command. (#496, @zshipko)

- **irmin-git**:
  - Fix `Node.add` to preserve sharing. (#802, @samoht)

- **irmin-http**:
  - Respond with a 404 if a non-existent resource is requested. (#706, @samoht)

- **irmin**:
  - Fix a bug whereby `S.History.is_empty` would return `true` for a store with
    exactly one commit. (#865, @pascutto)

### Removed

- **irmin**:
  - Remove `pp` and `of_string` functions from `Irmin.Contents.S` in favour of
    `Irmin.Type.to_string` and `Irmin.Type.of_string`.

  - Remove `Bytes` content type. (#708, @samoht)

  - Remove `Cstruct` dependency and content type. If possible, switch to
    `Irmin.Contents.String` or else use `Irmin.Type.map` to wrap the Cstruct
    type. (#544, @samoht)

## 1.4.0 (2018-06-06)

- Add types for `Contents.hash`, `Tree.hash` and `Commit.hash` (#512, @samoht)
- `Tree.hash` and `Tree.of_hash` now work on leaf nodes. To do this, `Tree.hash`
  has to return a more complex type (#512, @samoht)
- support for webmachine 0.6.0 (#505, @ansiwen)

## 1.3.3 (2018-01-03)

- complete support for OCaml 4.06 (#484, @samoht)
- support cohttp 1.0 (#484, @samoht)

## 1.3.2 (2017-11-22)

- support OCaml 4.06 where `-safe-string` is enabled by default (#477, @djs55)

## 1.3.1 (2017-08-25)

- irmin-http: update to cohttp.0.99 (#467, @samoht)

## 1.3.0 (2017-07-27)

**irmin-chunk**

Add a new package: `irmin-chunk`, which was initially in a separate repository
created by @mounirnasrallah and @samoht and ported to the new Irmin API by
@g2p (#464)

**irmin-unix**

Re-add the `irmin` binary, the example application which used to be
installed by irmin-unix` before we switched to use `jbuilder`
(#466, @samoht -- reported by @ouenzzo and @dudelson)

**irmin**

That releases saw a nice series of patches to improve the performance of
`Irmin.Tree` contributed by the Tezos team:

- Improve complexity of `Irmin.Tree` operations: on trivial benchmarks with
  a lot of values, this patch introduces a 10-times speed-up
  (#457, @OCamlPro-Henry)

- Add missing equality for `Irmin.Type` primitives (#458, @OCamlPro-Henry)

- Change the type of `Hash.digest` to also take a type representation
  (#458, @OCamlPro-Henry)

- add `Irmin.Type.{encode,decode}_cstruct` (#458, @OCamlPro-Henry)

- remove `Irmin.Contents.RAW` (#458, @OCamlPro-Henry)

- avoid unecessary serialization and deserialization when computing hashes
  of cstructs (#459, @OCamlPro-Henry)

- remove `{Type,Merge}.int` which might cause some issue on 32 bits platforms.
  Intead use the more explicit (and portable) `{Type,Merge}.int32` or
  `{Type,Merge}.int64` (#469, @samoht)

## 1.2.0 (2017-06-06)

This release changes the build system to use
[jbuilder](https://github.com/janestreet/jbuilder). By doing so, it introduces
two new packages: `irmin-mem` and `irmin-fs` -- containing `Irmin_mem` and
`Irmin_fs` respectively. That release also fixes a bunch of regressions
introduced in the big 1.0 rewrite.

**all**

- Use `jbuilder` (#444, @samoht)
- Use mtime 1.0 (#445, @samoht)

**irmin**

- Fix `Irmin.Contents.Cstruct`: pretty-print the raw contents, not the hexdump
  (#442, @samoht)
- `Irmin.Hash.X.of_string` should not raise an exception on invalid hash
  (#443, @samoht)

**irmin-mem**

- New package! Use it if you want to use the `Irmin_mem` module.

**irmin-fs**

- New package! Use it if you want to use the `Irmin_fs` module.

**irmin-git**

- Fix watches (#446, @samoht)

## 1.1.0 (2017-04-24)

**irmin**

- Change the type of `S.Tree.find_tree` to return a `tree option` instead of
  `tree`. This is a breaking API change but it let distinguish between
  the empty and non-existent cases (#431, @samoht)
- Allow to specify branches in urls for fetch using the `url#branch` syntax
  (#432, @samoht)
- Expose `Irmin.Merge.idempotent` for values with idempotent operations
  (#433, @samoht)
- Add a `S.repo` type as an alias to the `S.Repo.t` (#436, @samoht)
- Fix regression in `S.Tree.diff` intoduced in the 1.0 release: nested
  differences where reported with the wrong path (#438, @samoht)

**irmin-unix**

- Update to irmin.1.1.0 API changes (@samoht)

**irmin-git**

- Update to irmin.1.1.0 API changes (@samoht)

## 1.0.2 (2017-03-27)

**irmin**

- Add a cstruct type combinator (#429, @samoht)
- Fix regression introduced in 1.0.1 on merge of base buffers (strings,
  cstruct). For these types, updates are idempotent, e.g. it is fine
  if two concurrent branches make the same update. (#429, @samoht)

**irmin-unix**

- Add irminconfig man page (#427, @dudelson)

## 1.0.1 (2017-03-14)

**irmin**

- Default merge function should not assume idempotence of edits
  (#420, @kayceesrk)
- Wrap the merge functions for pair and triple with the default case.
  (#420, @kayceesrk)

**irmin-unix**

- Support all versions of cmdliner, 1.0.0 included (@samoht)

## 1.0.0 (2017-02-21)

Major API changes:

- It is now simpler to define mergeable contents, using new
  combinators to describe data-types (see `Type`).

- The mutable views have been replaced by immutable trees, and made
  first-class citizen in the API (see available `S.Tree`).
  Transactions now only ensure snapshot isolation instead of full
  serialisability.

- Creating a store with default path and branch implementations
  is now easier using the `KV` functors which just take one parameter:
  the contents.

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
* [api] add `KV_MAKER` to ease the creation of store with string lists
  as paths and strings as branches (#405, @samoht)

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
- Rename `S.Internals` into `S.Git` (#397, @samoht)
- Rename `S.Internals.commit_of_id` into `S.Git.git_commit` (#397, @samoht)
- Add `S.Git.of_repo` to convert an Irmin repo into a Git repo (#397, @samoht)
- Add `S.Git.to_repo` to convert a Git repo into an Irmin repo (#397, @samoht)
- Expose `S.Git_mem.clear` and `S.Git_mem.clear_all` for in-memory Git
  backends (#397, @samoht)
- Rename `Memory` into `Mem.Make` (#405, @samoht)
- Rename `FS` into `FS.Make` (#405, @samoht)
- Remove `CONTEXT` and fold it into `IO`  (#405, @samoht)
- Add `Mem.KV` and `FS.KV` to ease creatin of store with default
  implementations for branches and paths (#405, @samoht)
- Add `Mem.Ref` and `FS.Ref` access tags, remotes and other Git references
  (#407, @samoht)
- Allow to set-up a custom `.git` path (#409, @samoht)

**irmin-mirage**

- Adapt to Mirage3 (@hannesm, @yomimono, @samoht)
- Rename the `Task` module into `Info` to reflect the core API changes
- Change `Info.f` to accept an optional `author` argument and a format
  string as a message parameter (#261, #406 @samoht)
- Rename `Irmin_git` into `Git` (#405, @samoht)

**irmin-http**

- Remove the high-level HTTP API (#397, @samoht)
- Rewrite the low-level (backend) API using `ocaml-webmachine` (#397, @samoht)
- Add `KV` to ease creatin of store with default implementations for
  branches and paths (#405, @samoht)

**irmin-unix**

- Rename `Irmin_unix.task` into `Irmin_unix.info` (#397, @samoht)
- Remove `LOCK`  (#397, @samoht)
- Change `Irmin_unix.info` to take an optional `author` argument and accept
  a format string as message parameter (#261, #406 @samoht)
- Rename `Irmin_fs` into `FS` (#405, @samoht)
- Rename `Irmin_git` into `Git` (#405, @samoht)
- Rename `Irmin_http` into `Http` (#405, @samoht)

## 0.12.0 (2016-11-17)

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

## 0.11.1 (2016-06-14)

* Fix compilation of examples (#359, @samoht)

## 0.11.0 (2016-05-04)

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

## 0.10.1 (2015-11-26)

* Support for launchd: the `--address` argument of the CLI now
  supports a URI `launchd://<name>` where `<name>` corresponds
  to the section in the property list file (#321, by @djs55)
* Expose `/watch-rec` in the REST API (#326, by @samoht)
* Expose Store.Key = Contents.Path in Irmin.Maker. Otherwise,
  the type of steps is abstract. (#327, by @talex5)

## 0.10.0 (2015-10-14)

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

## 0.9.10 (2015-10-01)

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

## 0.9.9 (2015-08-14)

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

## 0.9.8 (2015-07-17)

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

## 0.9.7 (2015-07-06)

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

## 0.9.6 (2015-07-03)

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

## 0.9.5 (2015-06-11)

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

## 0.9.4 (2015-03-16)

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

## 0.9.3 (2015-01-04)

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

## 0.5.0 (2014-02-21)

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
