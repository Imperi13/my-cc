
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "str_dict.h"

// implementation of string_dictionary (key:string to value:"void *")
// currently,using hash-table
//

// simple rolling hash
static u_int64_t hash_string(char *str) {
  assert(str, "str is NULL");
  u_int64_t ret = 0;
  size_t len = strlen(str);
  for (size_t i = 0; i < len; i++) {
    ret *= 0x319735UL;
    ret += (u_int64_t)str[i];
  }

  return ret;
}

static u_int64_t hash_index(u_int64_t hash_val, size_t cap_order) {
  assert(0 < cap_order && cap_order <= 64, "invalid cap_order");
  u_int64_t ret =
      (u_int64_t)(0x6f9d01239b348125UL * hash_val) >> (64 - cap_order);
  assert(ret < (1UL << cap_order), "out of bounds");
  return ret;
}

#define DEFAULT_CAP_ORDER 4
#define RESIZE_THRESHOLD_PERCENT 70

typedef struct HashEntry {
  char *key;
  void *val;

  bool is_delete;
} HashEntry;

struct StrDict {
  size_t cap_order;
  size_t entry_cnt;
  HashEntry *entries;
};

static void resize(StrDict *dict) {
  StrDict new_dict;
  new_dict.cap_order = dict->cap_order + 1;
  new_dict.entry_cnt = 0;
  new_dict.entries = calloc(1UL << new_dict.cap_order, sizeof(HashEntry));

  // append dict entry
  for (size_t i = 0; i < (1UL << dict->cap_order); i++) {
    if (dict->entries[i].key && !dict->entries[i].is_delete) {
      add_str_dict(&new_dict, dict->entries[i].key, dict->entries[i].val);
    }
  }

  *dict = new_dict;
}

StrDict *new_str_dict(void) {
  StrDict *str_dict = calloc(1, sizeof(StrDict));
  str_dict->cap_order = DEFAULT_CAP_ORDER;
  str_dict->entry_cnt = 0;
  str_dict->entries = calloc(1UL << DEFAULT_CAP_ORDER, sizeof(HashEntry));

  return str_dict;
}

void *find_str_dict(StrDict *dict, char *name) {
  assert(dict, "dict is NULL");
  assert(name, "invalid key");

  if ((dict->entry_cnt * 100) / (1UL << dict->cap_order) >=
      RESIZE_THRESHOLD_PERCENT)
    resize(dict);

  u_int64_t hash_val = hash_string(name);
  u_int64_t index = hash_index(hash_val, dict->cap_order);

  while (1) {
    if (!dict->entries[index].key)
      return NULL;

    if (!dict->entries[index].is_delete &&
        strcmp(dict->entries[index].key, name) == 0)
      return dict->entries[index].val;

    index = (index + 1) % (1UL << dict->cap_order);
  }

  error("unreachable");
}

void add_str_dict(StrDict *dict, char *name, void *val) {
  assert(dict, "dict is NULL");
  assert(name, "invalid key");
  assert(val, "invalid val");
  assert(!find_str_dict(dict, name), "already exist in dict : \"%s\"", name);

  if ((dict->entry_cnt * 100) / (1UL << dict->cap_order) >=
      RESIZE_THRESHOLD_PERCENT)
    resize(dict);

  u_int64_t hash_val = hash_string(name);
  u_int64_t index = hash_index(hash_val, dict->cap_order);

  while (1) {
    if (!dict->entries[index].key) {
      dict->entries[index].key = name;
      dict->entries[index].val = val;
      dict->entry_cnt++;
      return;
    }

    index = (index + 1) % (1UL << dict->cap_order);
  }

  error("unreachable");
}

void remove_str_dict(StrDict *dict, char *name) {
  assert(dict, "dict is NULL");
  assert(name, "invalid key");
  assert(find_str_dict(dict, name), "not exist in dict : \"%s\"", name);

  u_int64_t hash_val = hash_string(name);
  u_int64_t index = hash_index(hash_val, dict->cap_order);

  while (1) {
    if (!dict->entries[index].is_delete &&
        strcmp(dict->entries[index].key, name) == 0) {
      dict->entries[index].is_delete = true;
      return;
    }

    index = (index + 1) % (1UL << dict->cap_order);
  }

  error("unreachable");
}
