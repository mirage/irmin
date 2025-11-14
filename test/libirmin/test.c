#include "irmin.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "greatest.h"

TEST test_irmin_value_json(void) {
  AUTO IrminType *json = irmin_type_json();
  IrminValue *j1 = irmin_value_of_string(json, "{\"a\": 1}", -1);
  ASSERT_NEQ(j1, NULL);
  irmin_value_free(j1);

  IrminValue *j2 = irmin_value_of_string(json, "{\"a\": 1", -1);
  ASSERT_EQ(j2, NULL);

  PASS();
}

TEST test_irmin_store(IrminConfig *config) {
  // Initialize repo and store
  AUTO IrminRepo *repo = irmin_repo_new(config);
  AUTO Irmin *store = irmin_main(repo);

  // Create new string value
  AUTO IrminString *a = irmin_string_new("123", 3);

  // Create path: a/b/c
  char *k[] = {"a", "b", "c", NULL};
  AUTO IrminPath *path = irmin_path(repo, k);

  // Create commit info
  AUTO IrminInfo *info = irmin_info_new(repo, "test", "set");

  // Set a/b/c to "123"
  ASSERT(irmin_set(store, path, (IrminContents *)a, info));
  ASSERT(irmin_mem(store, path));

  // Get a/b/c from store
  AUTO IrminString *v = (IrminString *)irmin_find(store, path);
  ASSERT_NEQ(v, NULL);

  // Get string representation
  uint64_t length = irmin_string_length(v);
  ASSERT_EQ(strncmp(irmin_string_data(v), irmin_string_data(a), length), 0);

  // Check that tree exists at a/b
  AUTO IrminPath *path1 = irmin_path_of_string(repo, "a/b", -1);
  ASSERT(irmin_mem_tree(store, path1));

  // Get tree at a/b
  AUTO IrminTree *t = irmin_find_tree(store, path1);

  // Set d to "456"
  AUTO IrminPath *path2 = irmin_path_of_string(repo, "d", 1);
  AUTO IrminString *b = irmin_string_new("456", -1);
  irmin_tree_add(repo, t, path2, (IrminContents *)b, NULL);
  ASSERT(irmin_tree_mem(repo, t, path2));

  // Commit updated tree
  info = irmin_realloc(info, irmin_info_new(repo, "test", "tree"));
  irmin_set_tree(store, path1, t, info);

  // Ensure the store contains a/b/d
  AUTO IrminPath *path3 = irmin_path_of_string(repo, "a/b/d", -1);
  ASSERT(irmin_mem(store, path3));

  // Big string
  size_t size = 1024 * 1024 * 64;
  char *src = malloc(size);
  memset(src, 'a', size);
  AUTO IrminContents *big_string =
      (IrminContents *)irmin_value_string(src, size);
  AUTO IrminInfo *info2 = irmin_info_new(repo, "test", "big_string");
  ASSERT(irmin_set(store, path3, big_string, info2));
  AUTO IrminString *big_string_ = (IrminString *)irmin_find(store, path3);
  ASSERT_EQ(irmin_string_length(big_string_), size);
  ASSERT_EQ(strncmp(irmin_string_data(big_string_), src, size), 0);
  free(src);

  // List
  IrminPathArray *paths = irmin_list(store, path1);
  ASSERT_EQ(irmin_path_array_length(repo, paths), 2);

  // Fetch
  AUTO IrminRepo *repo1 = irmin_repo_new(config);
  AUTO Irmin *store1 = irmin_main(repo1);
  AUTO IrminRemote *remote = irmin_remote_store(store);
  AUTO IrminCommit *c = irmin_fetch(store1, -1, remote);
  ASSERT(c);
  ASSERT(irmin_mem(store1, path3));

  // Push
  info = irmin_realloc(info, irmin_info_new(repo1, "test", "push"));
  ASSERT(irmin_remove(store1, path3, info));
  c = irmin_realloc(c, irmin_push(store1, -1, remote));
  ASSERT(c);

  ASSERT_FALSE(irmin_mem(store1, path3));

  PASS();
}

TEST test_irmin_store_git(void) {
  AUTO IrminConfig *config = irmin_config_git_mem(NULL);
  return test_irmin_store(config);
}

TEST test_irmin_store_fs(void) {
  AUTO IrminConfig *config = irmin_config_fs("sha1", "string");
  return test_irmin_store(config);
}

TEST test_irmin_tree(void) {
  // Setup config
  AUTO IrminConfig *config = irmin_config_mem(NULL, NULL);

  // Initialize repo and store
  AUTO IrminRepo *repo = irmin_repo_new(config);
  AUTO Irmin *store = irmin_main(repo);

  AUTO IrminTree *tree = irmin_tree_new(repo);

  AUTO IrminPath *p1 = irmin_path_of_string(repo, "a/b/c", -1);
  AUTO IrminContents *v1 = irmin_contents_of_string(repo, "1", -1);
  ASSERT(irmin_tree_add(repo, tree, p1, v1, NULL));

  AUTO IrminPath *ab = irmin_path_parent(repo, p1);
  ASSERT(irmin_tree_mem_tree(repo, tree, ab));
  AUTO IrminContents *v2 = irmin_tree_find(repo, tree, p1);
  ASSERT_NEQ(v2, NULL);

  AUTO IrminType *ty = irmin_type_contents(repo);
  ASSERT(irmin_value_equal(ty, (IrminValue *)v1, (IrminValue *)v2));

  AUTO IrminPath *empty = irmin_path_empty(repo);
  AUTO IrminInfo *info = irmin_info_new(repo, "test", "tree a/b/c");
  ASSERT(irmin_set_tree(store, empty, tree, info));
  ASSERT(irmin_tree_mem(repo, tree, p1));

  AUTO IrminKindedKey *key = irmin_tree_key(repo, tree);
  ASSERT_NEQ(key, NULL);
  ASSERT(irmin_kinded_key_is_node(repo, key));

  AUTO IrminHash *hash = irmin_tree_hash(repo, tree);
  ASSERT_NEQ(hash, NULL);

  AUTO IrminTree *tree1 = irmin_tree_of_key(repo, key);
  ASSERT_NEQ(tree1, NULL);

  AUTO IrminTree *tree2 = irmin_tree_of_key(repo, key);
  ASSERT_NEQ(tree2, NULL);

  AUTO IrminType *ty1 = irmin_type_tree(repo);
  ASSERT(irmin_value_equal(ty1, (IrminValue *)tree1, (IrminValue *)tree2));

  ASSERT_FALSE(irmin_repo_has_error(repo));

  PASS();
}

GREATEST_MAIN_DEFS();

int main(int argc, char *argv[]) {
  GREATEST_MAIN_BEGIN();
  irmin_log_level("error");
  RUN_TEST(test_irmin_value_json);
  RUN_TEST(test_irmin_store_git);
  RUN_TEST(test_irmin_store_fs);
  RUN_TEST(test_irmin_tree);
  caml_shutdown();
  GREATEST_MAIN_END();
}
