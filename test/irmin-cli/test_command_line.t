Create default config file
  $ cat > irmin.yml <<EOF
  > root: ./test
  > store: pack
  > EOF

Set a/b/c => 123 in ./test
  $ irmin set a/b/c 123
  irmin: [WARNING] Updating the control file to [Used_non_minimal_indexing_strategy]. It won't be possible to GC this irmin-pack store anymore.

Set foo => bar in ./test1
  $ irmin set --root ./test1 foo bar
  irmin: [WARNING] Updating the control file to [Used_non_minimal_indexing_strategy]. It won't be possible to GC this irmin-pack store anymore.

Check for a/b/c in ./test
  $ irmin get --root ./test a/b/c
  123

Check for the non-existence of foo in ./test
  $ irmin get foo
  <none>
  [1]

Check for foo in ./test1
  $ irmin get --root ./test1 foo
  bar

Try getting foo from ./test1 using the wrong store type
  $ irmin get --root ./test1 -s fs foo
  <none>
  [1]

Set d/e/f => 456 in ./test
  $ irmin set d/e/f 456

List keys in ./test
  $ irmin list /
  DIR a
  DIR d

List keys in ./test but no path is specified
  $ irmin list
  DIR a
  DIR d

Set g/h/i => 789 in ./test
  $ irmin set /g/h/i/ 789

Snapshot main branch
  $ export SNAPSHOT=`irmin snapshot`

List keys under g/h in ./test
  $ irmin list g/h
  FILE i

Remove g in ./test
  $ irmin remove g

List keys under g in ./test
  $ irmin list g

Load snapshot commit as store
  $ irmin get --commit $SNAPSHOT g/h/i
  789

Restore snapshot
  $ irmin revert $SNAPSHOT

Get g/h/i in ./test
  $ irmin get g/h/i/
  789

Create branch a from main
  $ irmin merge --branch a main

Remove g/h/i in branch a
  $ irmin remove --branch a /g/h/i/

Merge branch a in main
  $ irmin merge a

Check that g/h/i has been deleted after merge
  $ irmin get g/h/i/
  <none>
  [1]

Check mismatched hash function
  $ irmin set --root ./test-hash -s fs -h sha1 abc 123
  $ irmin snapshot --root ./test-hash -s fs -h blake2b 2> /dev/null
  [1]

Clone a local repo
  $ irmin clone --root ./cloned ./test
  irmin: [WARNING] Updating the control file to [Used_non_minimal_indexing_strategy]. It won't be possible to GC this irmin-pack store anymore.
  $ irmin get --root ./cloned a/b/c
  123

Show documentation
  $ irmin
  usage: irmin [--version]
               [--help]
               <command> [<args>]
  
  The most commonly used subcommands are:
      init        Initialize a store.
      get         Read the value associated with a key.
      set         Update the value associated with a key.
      remove      Delete a key.
      list        List subdirectories.
      tree        List the store contents.
      clone       Copy a remote respository to a local store
      fetch       Download objects and refs from another repository.
      merge       Merge branches.
      pull        Fetch and merge with another repository.
      push        Update remote references along with associated objects.
      snapshot    Return a snapshot for the current state of the database.
      revert      Revert the contents of the store to a previous state.
      watch       Get notifications when values change.
      dot         Dump the contents of the store as a Graphviz file.
      graphql     Run a graphql server.
      server      Run irmin-server.
      options     Get information about backend specific configuration options.
      branches    List branches
      log         List commits
  
  See `irmin help <command>` for more information on a specific command.
