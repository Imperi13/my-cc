#pragma once

typedef enum TypeKind {
  VOID,

  // integer or scalar
  LONGLONG,
  LONG,
  INT,
  SHORT,
  CHAR,
  BOOL,
  PTR,

  // floating-point
  FLOAT,
  DOUBLE,

  // aggregate
  STRUCT,
  UNION,
  FUNC,
  ARRAY,
} TypeKind;

typedef struct Type Type;

#include "analyze.h"
#include "parse.h"
#include "vector.h"

struct Type {
  TypeKind kind;

  // for integer type
  bool is_unsigned;

  // for FUNC
  Type *return_type;
  bool has_arg;
  bool has_variable_arg;
  Vector *args_vector;

  // for PTR,ARRAY
  Type *ptr_to;

  // for ARRAY
  int arr_size;
  bool is_null_size;

  // for STRUCT
  StructDef *st_def;

  // for UNION
  UnionDef *union_def;
};

extern Type type_void;
extern Type type_bool;

extern Type type_longlong;
extern Type type_ulonglong;
extern Type type_long;
extern Type type_ulong;
extern Type type_int;
extern Type type_uint;
extern Type type_short;
extern Type type_ushort;
extern Type type_char;
extern Type type_uchar;

extern Type type_double;
extern Type type_float;

// type alias
#define type_ptrdiff_t type_long
#define type_size_t type_ulong

Type *newtype_ptr(Type *type);
Type *newtype_struct(StructDef *st_def);
Type *newtype_union(UnionDef *union_def);

void builtin_type_init(Analyze *state);

Type *gettype_decl_spec(DeclSpec *decl_spec);
Type *gettype_declarator(Declarator *declarator, Type *base_type);
char *getname_declarator(Declarator *declarator);
Tree *getargs_declarator(Declarator *declarator);
ArrayDeclarator *get_arr_declarator(Declarator *declarator);

int integer_rank(Type *type);
Type *get_integer_promoted_type(Type *integer_type);
Type *get_arithmetic_converted_type(Type *lhs_type, Type *rhs_type);

int type_size(Type *type);
int type_alignment(Type *type);

bool is_arithmetic(Type *type);
bool is_integer(Type *type);
bool is_floating_point(Type *type);
bool is_scalar(Type *type);
bool is_same_type(Type *a, Type *b);
bool is_compatible(Type *a, Tree *b);
