# libirmin

`libirmin` provides C bindings to the irmin API.

## Installation

To install from the root of this repo:

```
$ opam pin add libirmin .
```

After installing `libirmin.so` can be found in `$OPAM_SWITCH_PREFIX/libirmin/lib`
and `irmin.h` will be in `$OPAM_SWITCH_PREFIX/libirmin/include`

This means when compiling programs that use `libirmin` you will need to include those
directories:

```
$ export IRMIN_CFLAGS=-I$OPAM_SWITCH_PREFIX/libirmin/include
$ export IRMIN_LDFLAGS=-L$OPAM_SWITCH_PREFIX/libirmin/lib -lirmin
$ cc $IRMIN_CFLAGS my-program.c -o my-program $IRMIN_LDFLAGS
```

## Usage examples

### Opening a store

The first thing you will need to do is configure a backend:

Using `irmin-git`:

```c
IrminConfig *config = irmin_config_git("string");
if (config == NULL){
  // Print error message
  IrminString *err = irmin_error_msg();
  fputs(irmin_string_data(err), stderr);
  irmin_string_free(err);
}
```

When using `irmin-mem`, `irmin-fs` or `irmin-pack` you can specify the hash type
in addition to the content type:

```c
IrminConfig *config = irmin_config_mem("sha256", "string");
```

Available backends: `irmin_config_mem`, `irmin_config_git`, `irmin_config_git_mem`, `irmin_config_fs`,
`irmin_config_pack`, `irmin_config_tezos`

If `NULL` is passed for the content parameter then `string` will be used by default and
when `NULL` is passed for the hash argument `blake2b` is used.

Available content types: `string`, `json` (JSON objects), `json-value` (any JSON value)
Available hash types: `blake2b`, `blake2s`, `rmd160`, `sha1`, `sha224`, `sha256`, `sha384`,
`sha512`, `tezos`

The `IrminConfig*` value should eventually be freed using `irmin_config_free`.

When using a backend that stores information on disk, you will probably want to set the `root` parameter:

```c
assert(irmin_config_set_root(config, "path/to/store"));
```

Next you can initialize the repo:

```c
IrminRepo *repo = irmin_repo_new(config);
if (repo == NULL){
  // handle error
}
```

From there you can create a store using the main branch:

```c
Irmin *store = irmin_main(repo);
```

Or from your branch of choice:

```c
Irmin *store = irmin_of_branch(repo, "my-branch");
```

### Cleanup

Every `IrminX` type should be released using the matching `irmin_X_free` function:

```c
irmin_free(store);
irmin_repo_free(repo);
irmin_config_free(config);
```

### Setting values

Setting a value when using string contents:

```c
IrminString *value = irmin_string_new("Hello, world!", -1);
IrminPath *path = irmin_path_of_string(repo, "a/b/c");
IrminInfo *info = irmin_info_new(repo, "author", "commit message");
assert(irmin_set(store, path, (IrminContents*)value, info));
irmin_info_free(info);
irmin_path_free(path);
irmin_string_free(value);
```

The `-1` argument to `irmin_string_new` is used to pass the length of
the string. If it ends with a NULL byte then passing `-1` will auto-
matically detect the string length. This also shows that `IrminString`
can be cast to `IrminContents` safely when using `string` contents.

When using `json` contents:

```c
IrminType *t = irmin_type_json();
IrminContents *value = irmin_contents_of_string(t, "{\"foo\": \"bar\"}", -1);
IrminPath *path = irmin_path_of_string(repo, "a/b/c");
IrminInfo *info = irmin_info_new(repo, "author", "commit message");
assert(irmin_set(store, path, value, info));
irmin_info_free(info);
irmin_path_free(path);
irmin_contents_free(value);
irmin_type_free(t);
```

### Getting values

The following will get the value associated with a path and print its string
representation to stdout:

```c
IrminPath *path = irmin_path_of_string(repo, "a/b/c");
IrminContents *value = irmin_find(store, path);
if (value != NULL){
  // value exists, print it to stdout
  IrminString *s = irmin_contents_to_string(repo, value);
  puts(irmin_string_data(s));
  irmin_string_free(s);
  irmin_contents_free(value);
}
irmin_path_free(path);
```

### Trees

Working with trees is similar to working with stores, only you will be using the
`irmin_tree_X` functions:

```c
IrminTree *tree = irmin_tree_new(repo);
IrminString *value = irmin_string_new("Hello, world!", -1);
IrminPath *path = irmin_path_of_string(repo, "a/b/c");
assert(irmin_tree_add(tree, path, value, NULL); // The NULL parameter here is for
                                                // metadata and will typically be
                                                // NULL

IrminPath *empty_path = irmin_path_empty();
IrminInfo *info = irmin_info_new(repo, "author", "commit message");
irmin_set_tree(store, empty_path, tree, info);

irmin_string_free(value);
irmin_path_free(path);
irmin_path_free(empty_path);
irmin_info_free(info);
irmin_tree_free(tree);
```

