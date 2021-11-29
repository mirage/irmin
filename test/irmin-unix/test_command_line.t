Create default config file
  $ cat > irmin.yml <<EOF
  > root: ./test
  > store: pack
  > EOF

Set a/b/c => 123 in ./test
  $ irmin set a/b/c 123

Set foo => bar in ./test1
  $ irmin set --root ./test1 foo bar

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
  $ irmin get --root ./test1 -s irf foo
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
  $ irmin set --root ./test-hash -s irf -h sha1 abc 123
  $ irmin snapshot --root ./test-hash -s irf -h blake2b
  irmin: [ERROR] Irmin_fs.value invalid hash size
  ERROR: (Invalid_argument "Irmin.head: no head")
  [1]
