  $ echo "root: ./test" > irmin.yml
  $ echo "store: pack" >> irmin.yml
  $ irmin set abc 123
  $ irmin set --root ./test1 foo bar
  $ irmin get abc
  123
  $ irmin get foo
  <none>
  [1]
  $ irmin get --root ./test1 foo
  bar
  $ irmin get --root ./test1 -s irf foo
  <none>
  [1]
