#pragma once

typedef struct Analyze Analyze;
typedef struct Obj Obj;
typedef struct ObjScope ObjScope;
typedef struct TypedefScope TypedefScope;
typedef struct Typedef Typedef;
typedef struct LabelScope LabelScope;
typedef struct SwitchScope SwitchScope;
typedef struct StructDef StructDef;
typedef struct Member Member;

#include "parse.h"
#include "type.h"

struct Analyze {
  Obj *glb_objs;
  Obj *current_func;
  LabelScope *break_labels;
  LabelScope *continue_labels;
  SwitchScope *switch_stmts;
  int label_cnt;
  StructDef *glb_stdefs;
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

struct StructDef {
  char *st_name;
  int st_len;

  bool is_defined;
  int size;
  int alignment;

  Member *members;

  // for linked-list
  StructDef *next;
};

struct Member {
  char *member_name;
  int member_len;

  Type *type;
  int offset;

  // for linked-list
  Member *next;
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

struct SwitchScope {
  Tree *switch_node;
  SwitchScope *next;
};

void analyze_translation_unit(Tree *ast);
int eval_constexpr(Tree *expr);
int calc_rbp_offset(int start, int data_size, int alignment);
