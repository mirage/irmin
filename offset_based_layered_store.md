# Offset Based Layered Pack Store

This document specifies an alternative approach to the problem of layering an irmin-pack store.

[The original approach](https://gist.github.com/icristescu/1afb7f9f862f8e989b8b6c195908e7d0) is referred to as _graph based layered store_ in this document, while the approach presented here is referred to as _offset based layered store_.

From the _graph based layered store_ we learned a few things:
- on disk _object graph traversal_ is expensive,
- index building is expensive and
- the _rw thread_ may insert _newies_ quicker than the freeze thread can deal with.

The design of the _offset based layered store_ aims to overcome the above-mentioned problems by exploiting the following techniques:
- no hash consing of node/blobs. It was made possible by structured keys and we discovered it is viable because new entries in Tezos rarely hash-cons with the past,
- _irmin-pack_ stores may have a starting offset that is not 0 and
- a sigle dict for the whole layered store.

Inside an _offset based layered store_ the binary encoded objects are portable between _layers_. Thanks to this feature it is possible to copy-paste bytes instead of decode-encode objects.

A freeze in an _offset based layered store_ moves to the _next upper_ a suffix of the _current upper_ that is _self contained_ except for the objects in the tree of the first commit. Thanks to this feature there is no need to perform an _object graph traversal_ on the section of the pack file that is heading to the _next upper_.


## Terminology
- `graph based layered store`: The existing layered store implementation that relies on `object graph traversal` to move data from `layer` to `layer`.
- `offset based layered store`: The layered store presented in this document. It relies on pack file offsets to move data from `layer` to `layer`.
- `cup`: Current UPper `layer` of a layered store. Always exists.
- `nup`: Next UPper `layer` of a layered store. Only exists during a freeze. Is made `cup` at the end of a freeze.
- `low`: LOWer `layer` of a layered store. May be non-existent. May be cleared in order to reclaim disk space.
- `generic pack store`: A data structure as defined in `irmin-pack/pack_store_intf.ml`. All of `cup`, `nup`, `low` and the layered store itself are `generic pack stores`.
- `layer`: One of the 3 irmin-pack store of a layered store. I.e.`cup`, `nup` and `low`.
- `newies`: Entries entering a layered store during a freeze.
- `freeze commit`: The commit picked by the user to be the axis of a freeze. At the end of the freeze, it will be contained in `nup`.
- `leftmost commit`: The commit object that is the very first entry of a pack file and that was the `freeze commit` during a freeze. It is non-existent if the pack file starts at offset 0. 
- `oldies entries`: The node+blob predecessors of a `freeze commit`. They are stored in a special KV store that is built during a freeze --- the `oldies store`.
- `oldies store`: A data structure containing the `oldies entries`, which are the node+blob predecessors of a `leftmost commit`. The implementation details of `oldies store` is out of the scope of this document.
- `up`: Data traveling from `cup` to `nup`.
- `down`: Data traveling from `cup` to `low`.
- `left`: Objects to the left of the `freeze commit` (i.e heading `down`).
- `right`: The `freeze commit` and the objects to its right (i.e. heading `up`).
- `undereferencable key`: Either an `Indexed` key containing a dangling hash, or a `Direct` key containing an offset that is both out of bounds (of the pack files) and unknown (to the `oldies stores`).
    - A key is `dereferencable` in the layered store iff it is `dereferencable` in `nup` or `low` (or both).
- `self contained`: Is said of a `generic pack store` to indicate that the entries it contains don't reference `undereferencable keys`. Examples:
    - at the end of a freeze, `nup` is almost `self contained` except for the commit parents of the `leftmost commit`,
    - `low` is always `self contained` if it has never been cleared,
    - a layered store is always `self contained` if it is never cleared,
    - only a commit key can legally be `undereferencable`.
- `SWMR`: Single Writer Multiple Reader. Is said of a database that support that form of concurrent interractions. The readers just need access to the file system. Relative to the _rw_ repo, an _ro_ repo can live in the same domain or not, in the same process or not, on the same machine or not (when shared file system). This property involves complex concurrent interactions that need to be thoroughly accounted for.
- `freezing`: Is said of the files of a layered store that appear to be part of an in-progress freeze or part of a freeze that crashed (the 2 situtations are indistiguishible). `nup` is present iff the layered store is `freezing`. As opposed to the _graph based layered store_, there is no need for a "lock" file.
- `rw after recovery`: A third opening mode for the layered stores (i.e. with _ro_ and _rw_). In this mode the opening routine expects the layered store to be in a `freezing` state. The opening routine will first mend the files and then yield to the _rw_ opening routine.
- `object graph traversal`: A traversal of a `generic pack store`, one entry at a time, from successors to predecessors, on a transitive closure (i.e. on a section of the graph bounded by 1+ objects above and 0+ object below).
- `rw thread` the thread that launches the freeze and that may push `newies` during the freeze.
- `type M`: The type of freeze that moves files from `cup` to build `low`.
- `type A`: The type of freeze that appends to an existing `low`.
- `belong` / `apparent`: When the tree of a commit is defined, some parts of the tree (or the totality of it) may be shared with older commits -- the apparent objects -- the others objects are said to belong to the commit. 
- `commit pointer` / `tree pointer`: The 2 main types of pointers between objects.
    - `commit pointer` for `commit -> commit`.
    - `tree pointer` for `commit -> inode`, `inode -> inode` and `inode -> blob`.

## Assumptions and Constrains
- A pack file can function with a starting offset that is not 0.
- Garbage collection in the layered store is purely offset based, not transitive graph based.
- A single dict is global to the layered store.
- Nodes never reference children by hash.
- The _layers_ can't be individually opened as irmin-pack stores.
- Concurrently opening twice a layered store in _rw_ mode is undefined.

See the `Rules Regarding Backpointer` section for 4 additional assumptions.

Depending on the type of branch store chosen, some assumptions may apply. See `The Branch Store` section.

The 4 following assumptions could be lifted with a bit more work on the specifications.

- The lower store can't be cleared during a freeze.
- The layered store API doesn't offer a way of aborting freezes.
- Crashing during a recovery leaves the files in an undefined state.
- Having an _ro_ repo open during a recovery is an undefined behaviour.

## High level workflow in 6 steps
---

#### 1st freeze - Before

![1a](https://i.imgur.com/Om46Lmk.png)

This series of examples starts with a regular irmin-pack store as it exists today (almost, see the migration chapter). It comprises an index and a pack file. It is in the _cup_ slot of the layered store.

Since the _minimal_ strategy is enforced, the index only contains the location of commits.

The pack file mininal offset is 0.

---

#### 1st freeze - Process

![1b](https://i.imgur.com/DvxLeJB.png)

This very first freeze operates on a non-existent _low_ and a _cup_ starting at offset 0. It makes it slightly simpler compared to the ones showcased after.

Freezes operating on a non-existent _low_ are said to be of _type M_ (as in move, as opposed to append). The benefit of the _type M_ freeze is that it is extremely efficient to deal with the initial freeze on Tezos archive node.

Before the freeze starts, the Irmin user specifies a freeze axis in the form of a `commit_key`.

##### pack file

In the pack file, the bytes left of the freeze axis (blue) will be moved to _low_ and the bytes right or at the freeze axis (yellow) will be moved to _nup_. As a result, the freeze commit object will be copied to _nup_'s pack file and its predecessors will be copied to _low_'s pack file. 

There is no need to perform any kind of decoding, only copy.

##### index

In the index, all entries referencing commits left of the freeze axis will be moved into _low_ and all the entries referencing commits right or at the freeze axis will be moved into _nup_. As a result, the offset of the _freeze commit_ will be inserted into _nup_'s index.

##### _newies_

A freeze should be mindful of objects that make it into the layered store during the freeze itself.

To deal with the pack file _newies_, all the bytes that are written to _cup_ are also forwarded to _nup_ directly at the offset where they are expected to live by the end of the freeze. As a result, _nup_'s pack file will be concurrently filled by the _rw thread_ and the freeze thread on two non-overlapping sections.

In other words, the _rw thread_ itself takes care of its own pack file _newies_.

A similar scheme is applied for the newies in index (see `The Freeze Algorithm` section for more details).

##### oldies

As the _freeze commit_ will be the leftmost object of _nup_'s pack file, its node+blob predecessors will not make it into that same pack file. In order to make _nup_ _self contained_, a new on-disk data structure has to be introduced in order to store these nodes and blobs: the _oldies store_.

_oldies store_ is a KV datastructure that is _rw_ during the freeze and _ro_ afterwards. It specialises in storing pack entries with non-contiguous offsets. The keys are pack file offsets and the values are objects. One very unefficient way of implementing it is as a sparse pack file.

Building _nup_'s _oldies store_ requires traversing some of _cup_'s entries, starting from the root node of the _freeze commit_.

---

#### 2nd freeze - Before
![2a](https://i.imgur.com/M2yMQvo.png)

At the end of the second freeze _nup_ became _cup_. After the freeze it grew again, and it is now time for a second freeze. This time the yellow bits will be moved to _low_ and the red to _nup_.


---

#### 2nd freeze - Process
![2b](https://i.imgur.com/rlo0JkA.png)

Freezes operating on a existent _low_ are said to be of _type A_ (as in append, as opposed to move).

_nup_ is built the same way as in the previous example. A small difference is that the entries that constitute _nup_'s _oldies store_ may not just come from _left_, they may come from _cup_'s _oldies store_.

The situtation for _low_ is very similar as in _type M_, except that instead of reusing _cup_'s pack file we will append to the existing _low_ pack file.

---

#### 3rd freeze - Before
![3a](https://i.imgur.com/Hnj6NvF.png)

After the end of the second freeze the user got rid of _low_ in order to reclaim disk space. The red bits will be moved to _low_ and the green ones to _nup_.

---

#### 3rd freeze - Process
![3b](https://i.imgur.com/m1CjAu7.png)

This last freeze is of _type M_ again. Its only difference with the first freeze is that _cup_ doesn't start at offset 0 and that it has an _oldies store_. That _oldies store_ should be moved to _low_.

---

## Random facts
- The irmin-pack keys (from irmin 3.0) work as is in that design.
    - The `Indexed` ones can be _dereferenced_ by first looking in _cup_ and then in _low_.
    - The `Direct` ones can be _dereferenced_ by comparing their offset to _cup_'s starting offset. If lower then _low_, else _cup_.
- The performance bottleneck is expected to be the _oldies store_ as the rest of the work is dealing with small files and sequential bytes copies from pack file to pack file.
- Only one _object graph traversal_ has to be performed per freeze (i.e. on _oldies_).
- Inode's `encode_bin` is never triggered by the freeze thread. All copies are bitwise.
- Inode's `decode_bin` is only triggered by the freeze thread for the traversal of _oldies_.
- Opening a _freezing_ store in _ro_ mode is always possible, no matter the precise state of the files.
- Opening a _freezing_ store in _rw_ mode is not possible.
- Opening a _freezing_ store in _rw after recovery_ mode is always possible, no matter the precise state of the files.
- _low_ is read only for the _rw thread_ (See `The Branch Store` section for subtleties).
- _cup_ is read only for the freeze thread.
- _nup_ is invisible to the read coming from the _rw thread_ or _ro_ repos.
- _cup_ has a starting offset of 0 iff it has an _oldies store_.
- Only one freeze can be a active at the same time.
- During a freeze, _low_ and _cup_ will overlap. Both their pack file and index store are concerned. Since _cup_ always has priority over _low_ when looking for a hash or an offset in the layered store, the content of the overlap in _low_ is never accessed by the exterior.
- A freeze is totally seemless from the point of view of the _rw thread_. There are as many entries in the layered store before and after a freeze (modulo the newies).
- The freeze should be performed from within the same memory space as the _rw thread_. It could be a separated OCaml thread, a separated Lwt thread or a separate domain.
- As opposed to the _graph based layered store_, there is no need for a `flip` file. The `./cup` directory is the upper to be used.
- As opposed to the _graph based layered store_ which uses two upper layers labelled `up0` and `up1`, here _cup_ and _nup_ don't refer to persistent stores that are cleared, new ones are created each time.

## Rules Regarding Backpointer
The backpointers in a pack file are offsets. These offsets are smaller than the position at which they are inserted.

While the predecessors of a _leftmost commit_ are explicitly dealt with by the freeze process, the fact that predecessors of the non-_leftmost commit_ should be contained in the layer is implicit in order to save computation time.

Handling this implicitly implies 4 rules (⚖️) which are realistic but very unsafe if not respected by the Irmin user. Handling this explicitly would be realistic too but would require the introduction of a complex component to the design in order to keep a low the overhead. This component is out of the scope of this document.

#### _Commit pointers_ of a non-_leftmost commit_
> ⚖️ __I__ - The _commit pointers_ of a non-_leftmost commit_ should be in _left_.

In other words, only the _freeze commit_ has commit predecessors outside of _right_.

This assumption may be violated in the unlikely event that Tezos chooses an orphan block as the _freeze commit_.

#### Location of objects _belonging_ to a non-_leftmost commit_
> ⚖️ __II__ - The objects _belonging_ to a commit are stored contiguously and immediately before the commit. Some dead objects that belong to no commit may also live inside such a contiguous section.

This rule is not enforced in irmin-pack but it is never violated in Tezos (even during the `H` migration). It would be violated if Tezos built 2 contexts in parallel while triggering the auto-flushes in an unlucky way.

#### Location of objects _apparent_ to a non-_leftmost commit_

There are 2 types of object sharing in a pack file. The "derived from" sharing, which naturally occur when a tree is derived from another, and the "hash consing" sharing, which is enabled by index. The first one has to be constrained while the second one has to be disabled.

###### _Apparent_ objects from tree derivation are in _left_
Since rule __I__ already forbids the non-_leftmost commit_ to have _commit pointers_ outside of _left_, most of the problem is already solved. However a loophole remains. A commit may fail to report a commit predecessor.

The _commit pointers_ are optional in Irmin. They exist in order to state a parenthood between commits. The most common parenthood to state is that a later commit was derived from a former one.

> ⚖️ __III__ - A non-_leftmost commit_ should be derived from trees of commits in _left_.

In other words, in addition to rule __I__ the commits should accurately list all the commits on which their tree was derived from.

This rule is enforced in Tezos.

###### No _apparent_ objects from hash consing
Hash consing of object would create tons of _tree pointers_ from _right_ to _left_. It should simply be disabled.

> ⚖️ __IV__ - The indexing strategy is minimal (i.e. all commits, nothing else).


## The Branch Store
WIP


## The Dict
The layered store uses a single global dict that is never garbage collected.

It is global in order to avoid having to decode all inodes of _cup_ on freeze.

In order to garbage collect it, we would have to implement a reference counting in it, and decrement the references on _low_ clear.


## Extension to Tezos Snapshotting

The act of taking a slice of _cup_ to create a _nup_ is very similar to the act of snapshot exporting and importing in Tezos. The freeze algorithm could share its code with a snapshot export function.

The snapshot export would directly create the bytes of the future files and the snapshot import would just have to push these bytes to the right files.

The snapshots produced that way are not canonical.


## Migration from irmin-pack `v2` to _offset based layered store_

```sh
mkdir cup
mv store.pack cup
mv index cup
# maybe move store.branches (See The Branch Store section)
# do not move store.dict
```

In addition to having to move files around, the migration would need to upgrade `store.pack` because the layered store design requires the pack files to explicitly store their starting offset (e.g in their header, in some other file).


## The Freeze Algorithm
> ###### Step 1 - prime
> - freeze thread under batch lock:
>   1. Take freeze lock
>   1. `mkdir nup` (and pack file/index)
>   1. (if type M) `mkdir tmp_low` (and pack file/index)
> - _rw thread_ is excluded.
>
> ###### Step 2 - _cup_'s index iter
> - freeze thread iterates on _cup_'s index to copy _up_ and _down_.
> - _rw thread_'s index newies are pushed in both _cup_ and in an ocaml list.
> - _rw thread_'s pack file newies are pushed in both _cup_ and _nup_.
> 
> ###### Step 3 - index newies so far
> - freeze thread takes batch lock and pushes the index list to _nup_.
> - _rw thread_ is excluded.
> 
> ###### Step 4 - oldies
> - freeze thread builds the oldies _up_.
> - _rw thread_'s index newies are pushed in both _cup_ and _nup_.
> - _rw thread_'s pack file newies are store in both _cup_ and _nup_.
>
> ###### Step 5 - pack file copies
> - freeze thread copies pack bytes _up_ (and _down_ if _type A_).
> - _rw thread_'s index newies are pushed in both _cup_ and _nup_.
> - _rw thread_'s pack file newies are store in both _cup_ and _nup_.
> 
> ###### Step 6 - swap (_type A_)
> - freeze thread under batch lock:
>     1. flush and close all files
>     1. `rm -rf cup` (first step of swap is to vanish _cup_)
>     1. `mv nup cup` (last step of swap is to vanish _nup_)
>     1. release freeze lock
> - _rw thread_ is excluded.
> 
> ###### Step 6 - swap (_type M_)
> - freeze thread under batch lock:
>     1. flush and close all files
>     1. `mv cup tmp_cup` (first step of swap is to vanish _cup_)
>     1. `mv tmp_cup/store.pack tmp_low`
>     1. `mv tmp_cup/store.oldies tmp_low`
>     1. `rm -rf tmp_cup`
>     1. `mv tmp_low low`
>     1. `mv nup cup` (last step of swap is to vanish _nup_)
>     1. release freeze lock
> - _rw thread_ is excluded.

When `Step 4` is reached there is no more need to worry about the _newies_ as they are directly forwarded to _nup_ by the _rw thread_. Since index doesn't support concurrent insertions, the "ocaml list" trick is necessary at `Step 2` while the index entries are copied _up_ by the freeze thread.

`Step 2` and `Step 3` will make _low_'s index reference offsets that are not contained in it the pack file. These should not be a problem for the other threads as _cup_'s index still possesses these hashes and that the `Pack.find` function should always look in _cup_ before _low_.

The freezing of the branch store depends on the type of branch store chosen. See the `The Branch Store` section.

## The Recovery Algorithm
In order to revive a layered store that crashed during a freeze, it must be open the `rw after recovery` mode. If the freeze crashed before a certain point, the recovery will cleanup the mess, after a certain point, the recovery will finish the freeze.

Cleaning the mess in _low_ implies:
- filtering out of index the entries with an offset higher or equal to _cup_'s starting offset
- updating the pack files end offset to _cup_'s starting offset

```py
freezing = nup on disk
if freezing:
    swap_began = cup not on disk
    if not swap_began:
        # abort the freeze
        cleanup_lower()
        rm nup
    else:
        # finish the freeze             # Step 2 of M and step 2 of A went through
        if tmp_cup/store.pack on disk:
            mv tmp_cup/store.pack low   # Step 3 of M 
        if tmp_cup/store.oldies on disk:
            mv tmp_cup/store.oldies low # Step 4 of M
        if tmp_cup on disk: 
            rm tmp_cup                  # Step 5 of M
        if tmp_low on disk:
            mv tmp_low low              # Step 6 of M
        mv nup cup                      # Step 7 of M and step 3 of A
else:
    raise "Nothing to recover from"
```


## Concurrency Consistencies
WIP


---

## Appendix - Alternative Naming
- Offset based layered pack store
- layered pack store with global offset
- layered pack store with shared offset
- layered pack store with persistent offset
- temporal layered pack store


## Appendix - Alternative Naming for `oldies store`
Instead of `oldies entry` and `oldies store` we could use `oldies` for the first and a new name for the later.

> souvenirs, archives, reminders, history, records, past, prehistory, saga, memoirs, ancestors, museum, vault, storehouse, cellar, basement, echo, redundance, copies, mirror, duplicates, replicas, surrogate, backup, __igloo__, refuge, bridge, overpass, splice

---
----
-----
