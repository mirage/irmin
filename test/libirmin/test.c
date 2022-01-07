#include "irmin.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void test_irmin_value_json(void) {
  puts("Running libirmin JSON value tests");
  AUTO IrminType *json = irmin_type_json();
  IrminValue *j1 = irmin_value_of_string(json, "{\"a\": 1}", -1);

  IrminString *err0 = irmin_error_msg();
  assert(err0 == NULL);
  assert(j1 != NULL);
  irmin_value_free(j1);

  IrminValue *j2 = irmin_value_of_string(json, "{\"a\": 1", -1);
  assert(j2 == NULL);
  AUTO IrminString *err = irmin_error_msg();
  assert(err != NULL);
}

void test_irmin_store(void) {
  puts("Running libirmin store tests");
  // Setup config for git store
  AUTO IrminConfig *config = irmin_config_git_mem(NULL);

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
  assert(irmin_set(store, path, (IrminValue *)a, info));
  assert(irmin_mem(store, path));

  // Get a/b/c from store
  AUTO IrminString *v = (IrminString *)irmin_find(store, path);
  assert(v);

  // Get string representation
  uint64_t length = irmin_string_length(v);
  assert(strncmp(irmin_string_data(v), irmin_string_data(a), length) == 0);

  // Check that tree exists at a/b
  AUTO IrminPath *path1 = irmin_path_of_string(repo, "a/b", -1);
  assert(irmin_mem_tree(store, path1));

  // Get tree at a/b
  AUTO IrminTree *t = irmin_find_tree(store, path1);

  // Set d to "456"
  AUTO IrminPath *path2 = irmin_path_of_string(repo, "d", 1);
  AUTO IrminString *b = irmin_string_new("456", -1);
  irmin_tree_add(repo, t, path2, (IrminValue *)b, NULL);
  assert(irmin_tree_mem(repo, t, path2));

  // Commit updated tree
  AUTO IrminInfo *info1 = irmin_info_new(repo, "test", "tree");
  irmin_set_tree(store, path1, t, info1);

  // Ensure the store contains a/b/d
  AUTO IrminPath *path3 = irmin_path_of_string(repo, "a/b/d", -1);
  assert(irmin_mem(store, path3));

  // Big string
  size_t size = 1024 * 1024 * 64;
  char *src = malloc(size);
  memset(src, 'a', size);
  AUTO IrminValue *big_string = irmin_value_string(src, size);
  AUTO IrminInfo *info2 = irmin_info_new(repo, "test", "big_string");
  assert(irmin_set(store, path3, big_string, info2));
  AUTO IrminString *big_string_ = (IrminString *)irmin_find(store, path3);
  assert(irmin_string_length(big_string_) == size);
  assert(strncmp(irmin_string_data(big_string_), src, size) == 0);
  free(src);

  // List
  IrminPathList *paths = irmin_list(store, path1);
  assert(irmin_path_list_length(repo, paths) == 2);
}

void test_irmin_tree(void) {
  puts("Running libirmin store tests");
  // Setup config for git store
  AUTO IrminConfig *config = irmin_config_git_mem(NULL);

  // Initialize repo and store
  AUTO IrminRepo *repo = irmin_repo_new(config);
  AUTO Irmin *store = irmin_main(repo);

  AUTO IrminTree *tree = irmin_tree_new(repo);

  AUTO IrminPath *p1 = irmin_path_of_string(repo, "a/b/c", -1);
  AUTO IrminValue *v1 = irmin_contents_of_string(repo, "1", -1);
  irmin_tree_add(repo, tree, p1, v1, NULL);

  AUTO IrminPath *ab = irmin_path_parent(repo, p1);
  assert(irmin_tree_mem_tree(repo, tree, ab));
  AUTO IrminValue *v2 = irmin_tree_find(repo, tree, p1);
  assert(v1);

  AUTO IrminType *ty = irmin_type_contents(repo);
  assert(irmin_value_equal(ty, v1, v2));

  AUTO IrminPath *empty = irmin_path_empty(repo);
  AUTO IrminInfo *info = irmin_info_new(repo, "test", "tree a/b/c");
  assert(irmin_set_tree(store, empty, tree, info));
  assert(irmin_tree_mem(repo, tree, p1));

  AUTO IrminKindedKey *key = irmin_tree_key(repo, tree);
  assert(key != NULL);
  assert(irmin_kinded_key_is_node(repo, key));

  AUTO IrminHash *hash = irmin_tree_hash(repo, tree);
  assert(hash != NULL);

  AUTO IrminTree *tree1 = irmin_tree_of_key(repo, key);
  assert(tree1 != NULL);

  AUTO IrminTree *tree2 = irmin_tree_of_key(repo, key);
  assert(tree2 != NULL);

  AUTO IrminType *ty1 = irmin_type_tree(repo);
  assert(irmin_value_equal(ty1, (IrminValue *)tree1, (IrminValue *)tree2));
}

int main(int argc, char *argv[]) {
  irmin_log_level("error");
  test_irmin_value_json();
  test_irmin_store();
  test_irmin_tree();
  puts("Finished libirmin test");
  return 0;
}
