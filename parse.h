#pragma once


typedef enum {
  FOR_STMT,
} TreeKind;

typedef struct Tree Tree;

struct Tree{
  // for linked-list
  Tree *next;
};

