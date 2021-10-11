Create default config file
  $ cat > irmin.yml <<EOF
  > root: ./test
  > store: pack
  > EOF

Set abc => 123 in ./test
  $ irmin set abc 123

Set foo => bar in ./test1
  $ irmin set --root ./test1 foo bar

Check for abc in ./test
  $ irmin get --root ./test abc
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
