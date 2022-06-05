#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  TK_RESERVED,
  TK_RETURN,
  TK_SIZEOF,
  TK_STRUCT,
  TK_IF,
  TK_ELSE,
  TK_DO,
  TK_WHILE,
  TK_FOR,
  TK_BREAK,
  TK_CONTINUE,
  TK_IDENT,
  TK_INT,
  TK_CHAR,
  TK_NUM,
  TK_STR,
  TK_EOF,
} TokenKind;

typedef struct Token Token;
typedef struct StrLiteral StrLiteral;

struct Token {
  TokenKind kind;
  Token *next;
  int val;
  char *str;
  int len;

  // for str-literal
  StrLiteral *str_literal;
};

struct StrLiteral {
  char *str;
  int len;
  int id;

  // for linked-list
  StrLiteral *next;
};

typedef enum {
  INT,
  CHAR,
  PTR,
  ARRAY,
  FUNC,
  STRUCT,
} TypeKind;

typedef struct Type Type;
typedef struct TypeList TypeList;

typedef struct StructDef StructDef;
typedef struct Member Member;

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
};

struct TypeList {
  TypeList *next;
  Type *type;
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
  ND_DO_WHILE,
  ND_WHILE,
  ND_FOR,
  ND_BREAK,
  ND_CONTINUE,
  ND_BLOCK,
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

  // for function
  NodeList *stmt_front;
  NodeList *stmt_back;
  char *name;
  int len;
  NodeList *expr_front;
  NodeList *expr_back;

  bool is_defined;

  // for struct-dot
  Member *member;
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
};

struct ObjList {
  ObjList *next;
  Obj *obj;
};

struct VarScope {
  VarScope *next;
  ObjList *locals;
};

extern const char variable_letters[];
extern Token *dummy_token;
extern Type *type_int;
extern Type *type_char;

extern char *filename;
extern char *user_input;
extern StrLiteral *str_literals;
extern StructDef *struct_defs;
extern ObjList *globals;
extern Obj *now_function;

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

bool equal(Token *token, char *op);
bool equal_kind(Token *token, TokenKind kind);
bool consume(Token **rest, Token *token, char *op);
Token *consume_kind(Token **rest, Token *token, TokenKind kind);
void expect(Token **rest, Token *token, char *op);
Token *expect_kind(Token **rest, Token *token, TokenKind kind);
int expect_number(Token **rest, Token *token);
bool at_eof(Token *token);

bool is_alnum(char c);

Token *tokenize(char *p);

void debug_token(Token *token);

Member *find_member(StructDef *st, char *name, int len);

Obj *parse_global_decl(Token **rest, Token *tok, bool lookahead);
Obj *parse_local_decl(Token **rest, Token *tok);
Type *type_name(Token **rest, Token *tok);
Type *newtype_ptr(Type *base);
bool is_numeric(Type *a);
bool is_same_type(Type *a, Type *b);
bool is_convertible(Type *a, Type *b);
int type_size(Type *a);
int type_alignment(Type *a);
int offset_alignment(int start, int data_size, int alignment);

Obj *find_obj(ObjList *list, char *str, int len);
Obj *find_lvar(char *str, int len);
void program(Token *tok);
Node *assign(Token **rest, Token *tok);

void codegen_all(FILE *output);
