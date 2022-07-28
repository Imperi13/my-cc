#pragma once

typedef struct Analyze Analyze;
typedef struct Obj Obj;
typedef struct ObjScope ObjScope;
typedef struct TypedefScope TypedefScope;
typedef struct Typedef Typedef;
typedef struct LabelScope LabelScope;

#include "parse.h"
#include "type.h"

struct Analyze {
  Obj *globals;
  Obj *current_func;
  LabelScope *break_labels;
  LabelScope *continue_labels;
  int label_cnt;
};

struct Obj {

  char *obj_name;
  int obj_len;

  Type *type;

  // for function
  ObjScope *locals;
  int stack_size;

  Obj *args;

  // for globals
  bool is_defined;
  bool is_global;

  // for local-var
  int rbp_offset;

  // for linked-list
  Obj *next;
  Obj *arg_next;
};

struct ObjScope {
  Obj *obj;

  // for linked-list
  ObjScope *next;
};

struct TypedefScope {
  Typedef *typedefs;

  // for linked-list
  TypedefScope *next;
};

struct Typedef {
  char *name;
  int len;

  // for linked-list
  Typedef *next;
};

struct LabelScope {
  int label_number;
  LabelScope *next;
};

void analyze_translation_unit(Tree *ast);
int offset_alignment(int start, int data_size, int alignment);
