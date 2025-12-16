  $ irmin set --plugin ./plugin.cmxs a/b/c 123
  $ echo 'plugin: plugin.cmxs' > irmin.yml # Set the plugin in config file
  $ irmin set --root ./irmin-plugin -s git -c int a/b/c 123
  $ irmin get --root ./irmin-plugin -s git -c int a/b/c
  123
  $ irmin set --root ./irmin-plugin -s git -c int a/b/c "AAA"
  ERROR: int_of_string
  [1]

