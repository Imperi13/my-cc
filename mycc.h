#pragma once

#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tokenize.h"

typedef enum {
  VOID,
  INT,
  CHAR,
  PTR,
  ARRAY,
  FUNC,
  STRUCT,
  ENUM,
} TypeKind;

typedef struct Type Type;
typedef struct TypeList TypeList;
typedef struct TypeQual TypeQual;

typedef struct StructDef StructDef;
typedef struct Member Member;

typedef struct EnumDef EnumDef;
typedef struct EnumConst EnumConst;
typedef struct ConstList ConstList;

struct Type {
  TypeKind ty;
  Type *ptr_to;
  size_t array_size;

  // for FUNC
  Type *return_type;
  TypeList *argtype_front;
  TypeList *argtype_back;
  size_t arg_size;

  // for struct
  StructDef *st;

  // for enum
  EnumDef *en;
};

struct TypeList {
  TypeList *next;
  Type *type;
};

struct TypeQual {
  bool is_const;
  bool is_extern;
  bool is_static;
};

struct StructDef {
  char *name;
  int len;
  bool is_defined;

  Member *members;
  int size;

  // for linked list
  StructDef *next;
};

struct Member {
  char *name;
  int len;
  Type *type;
  int offset;

  // for linked list
  Member *next;
};

struct EnumDef {
  char *name;
  int len;
  bool is_defined;

  ConstList *enum_consts;

  // for linked list
  EnumDef *next;
};

struct EnumConst {
  char *name;
  int len;
  int val;
};

struct ConstList {
  EnumConst *en_const;
  ConstList *next;
};

typedef enum {
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_LOGICAL_NOT,
  ND_BIT_NOT,
  ND_LSHIFT,
  ND_RSHIFT,
  ND_BIT_AND,
  ND_BIT_XOR,
  ND_BIT_OR,
  ND_LOGICAL_AND,
  ND_LOGICAL_OR,
  ND_CONDITIONAL,
  ND_COMMA,
  ND_DOT,
  ND_ARROW,
  ND_MOD,
  ND_EQUAL,
  ND_NOT_EQUAL,
  ND_GREATER,
  ND_GREATER_EQUAL,
  ND_SMALLER,
  ND_SMALLER_EQUAL,
  ND_ASSIGN,
  ND_ADD_ASSIGN,
  ND_POST_INCREMENT,
  ND_VAR,
  ND_STR,
  ND_ADDR,
  ND_DEREF,
  ND_RETURN,
  ND_IF,
  ND_IFELSE,
  ND_SWITCH,
  ND_DO_WHILE,
  ND_WHILE,
  ND_FOR,
  ND_BREAK,
  ND_CONTINUE,
  ND_DEFAULT,
  ND_CASE,
  ND_BLOCK,
  ND_LABEL,
  ND_FUNCTION_CALL,
  ND_NUM,
  ND_NOP,
} NodeKind;

typedef struct Node Node;
typedef struct NodeList NodeList;

struct Node {
  NodeKind kind;
  Type *type;
  Node *lhs;
  Node *rhs;
  int val;
  int offset;
  // for var
  bool is_global;

  // for str-literal
  StrLiteral *str_literal;

  // for conditional-stmt
  Node *expr;
  Node *init_expr;
  Node *update_expr;

  // for function call
  NodeList *stmt_front;
  NodeList *stmt_back;
  char *name;
  int len;
  NodeList *args;

  bool is_defined;

  int ret_offset;

  // for struct-dot
  Member *member;

  // for switch
  NodeList *case_nodes;
  Node *default_node;

  // for label
  char *label_name;
  int label_len;

  // for case
  int case_num;
};

struct NodeList {
  NodeList *next;
  Node *node;
};

typedef struct Obj Obj;
typedef struct ObjList ObjList;
typedef struct VarScope VarScope;

struct Obj {
  Type *type;
  char *name;
  int len;

  TypeQual *qual;

  // for lvar
  int offset;
  Node *init_var;

  // for function
  bool is_defined;
  Node *code;

  VarScope *local_scope;
  int stack_size;

  int arg_size;
  ObjList *arg_front;
  ObjList *arg_back;

  bool is_buf_return;
};

struct ObjList {
  ObjList *next;
  Obj *obj;
};

struct VarScope {
  VarScope *next;
  ObjList *locals;
};

extern Type *type_void;
extern Type *type_int;
extern Type *type_char;

extern StructDef *struct_defs;
extern EnumDef *enum_defs;
extern ConstList *enum_consts;
extern ObjList *globals;
extern Obj *now_function;


Member *find_member(StructDef *st, char *name, int len);
EnumConst *find_enum_const(char *name, int len);

Obj *parse_global_decl(Token **rest, Token *tok);
Obj *parse_local_decl(Token **rest, Token *tok);
bool is_decl_spec(Token *tok);
Type *type_name(Token **rest, Token *tok);
Type *newtype_ptr(Type *base);
bool is_numeric(Type *a);
bool is_same_type(Type *a, Type *b);
bool is_compatible(Type *a, Node *b);
bool is_void_ptr(Type *a);
bool is_null_ptr(Node *a);
int type_size(Type *a);
int type_alignment(Type *a);
int offset_alignment(int start, int data_size, int alignment);

bool is_constexpr(Node *a);
int eval_constexpr(Node *a);

Obj *find_obj(ObjList *list, char *str, int len);
Obj *find_lvar(char *str, int len);
void program(Token *tok);
Node *assign(Token **rest, Token *tok);

void codegen_all(FILE *output);
