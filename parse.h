#pragma once


typedef enum {
  FUNC_DEF,
  DECL,
  FOR_STMT,
} TreeKind;

typedef struct Tree Tree;

struct Tree{
  // for linked-list
  Tree *next;
};

