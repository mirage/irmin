# Example plugin

## Building

From the root of the irmin repository:

```sh
$ dune build ./examples/plugin/plugin.cmxs
```

## Usage

To load this plugin you can use the `--plugin` flag when using `irmin` (or
`dune exec ./src/irmin-unix/bin/main.exe` from the root of the irmin repo):

```sh
$ dune exec ./src/irmin-unix/bin/main.exe -- set --plugin _build/default/examples/plugin/plugin.cmxs a/b/c 123
```

By default this will use the `mem-int` store defined in [plugin.ml](https://github.com/mirage/irmin/blob/main/examples/plugin/plugin.ml)
since the `default` parameter is `true` when calling `Irmin_unix.Resolver.Store.add`.

It is still possible to select the store and content type after a plugin has
been loaded. To use the `int` content type with a git store you can run:

```sh
$ echo 'plugin: _build/default/examples/plugin/plugin.cmxs' > irmin.yml # Set the plugin in config file
$ dune exec ./src/irmin-unix/bin/main.exe -- set --root /tmp/irmin-plugin -s git -c int a/b/c 123
```

Since the `default` parameter is `true` when registering the content type using
`Irmin_unix.Resolver.Contents.add`, `int` contents will already be the default,
which means `-c int` could be left out.
