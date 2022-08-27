#pragma once

typedef struct Analyze Analyze;
typedef struct Obj Obj;
typedef struct ObjScope ObjScope;
typedef struct Typedef Typedef;
typedef struct LabelScope LabelScope;
typedef struct SwitchScope SwitchScope;
typedef struct StructDef StructDef;
typedef struct UnionDef UnionDef;
typedef struct Member Member;
typedef struct EnumDef EnumDef;

#include "parse.h"
#include "str_dict.h"
#include "type.h"

struct Analyze {
  // global scope
  Obj *glb_objs;
  StrDict *glb_struct_def_dict;
  StrDict *glb_union_def_dict;
  StrDict *glb_typedef_dict;
  EnumDef *glb_endefs;
  Obj *current_func;

  // local scope
  LabelScope *break_labels;
  LabelScope *continue_labels;
  SwitchScope *switch_stmts;
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

  // for arg
  int nth_arg;

  // for local-var
  int rbp_offset;

  // for linked-list
  Obj *next;
};

struct ObjScope {
  Obj *obj;

  // for linked-list
  ObjScope *next;
};

struct StructDef {
  char *st_name;

  bool is_defined;
  int size;
  int alignment;

  Member *members;
};

struct UnionDef {
  char *union_name;

  bool is_defined;
  int size;
  int alignment;

  Member *members;
};

struct Member {
  char *member_name;
  int member_len;

  Type *type;
  int offset;

  // for linked-list
  Member *next;
};

struct EnumDef {
  char *en_name;
  int en_len;

  bool is_defined;
  EnumVal *members;
  // for linked-list
  EnumDef *next;
};

struct Typedef {
  char *name;

  Type *type;
};

struct LabelScope {
  int label_number;
  LabelScope *next;
};

struct SwitchScope {
  Tree *switch_node;
  SwitchScope *next;
};

Analyze *new_analyze_state();

void analyze_translation_unit(Tree *ast);

Typedef *find_typedef(Analyze *state, char *typedef_name);

bool is_constexpr(Tree *expr);
int eval_constexpr(Tree *expr);
int calc_rbp_offset(int start, int data_size, int alignment);
