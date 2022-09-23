
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "str_dict.h"

// implementation of string_dictionary (key:string to value:"void *")
// currently,using linked-list
//
// TODO replace to hash list or self-balancing binary search tree
//

typedef struct StrDictList StrDictList;

struct StrDict {
  StrDictList *head;
};

struct StrDictList {
  char *name;
  void *val;

  // for linked-list
  StrDictList *next;
};

StrDict *new_str_dict(void) { return calloc(1, sizeof(StrDict)); }

void *find_str_dict(StrDict *dict, char *name) {
  assert(dict, "dict is NULL");
  assert(name, "invalid key");

  for (StrDictList *cur = dict->head; cur; cur = cur->next)
    if (strcmp(cur->name, name) == 0)
      return cur->val;
  return NULL;
}

void add_str_dict(StrDict *dict, char *name, void *val) {
  assert(dict, "dict is NULL");
  assert(name, "invalid key");
  assert(val, "invalid val");
  assert(!find_str_dict(dict, name), "already exist in dict : \"%s\"", name);

  StrDictList *node = calloc(1, sizeof(StrDictList));
  node->name = name;
  node->val = val;

  node->next = dict->head;
  dict->head = node;
}

void remove_str_dict(StrDict *dict, char *name) {
  assert(dict, "dict is NULL");
  assert(name, "invalid key");
  assert(find_str_dict(dict, name), "not exist in dict : \"%s\"", name);

  if (strcmp(dict->head->name, name) == 0) {
    dict->head = dict->head->next;
    return;
  }

  StrDictList *prev = dict->head;
  StrDictList *cur = dict->head->next;

  while (strcmp(cur->name, name) != 0) {
    prev = prev->next;
    cur = cur->next;
  }

  prev->next = cur->next;
}
