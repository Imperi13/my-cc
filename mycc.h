#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

typedef enum{
  INT,
  PTR,
  ARRAY,
  FUNC,
} TypeKind;

typedef struct Type Type;
typedef struct TypeList TypeList;

struct Type {
  TypeKind ty;
  Type *ptr_to;
  size_t array_size;

  // for FUNC
  Type *return_type;
  TypeList *argtype_front;
  TypeList *argtype_back;
};

struct TypeList {
  TypeList *next;
  Type *type;
};

typedef enum {
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_MOD,
  ND_EQUAL,
  ND_NOT_EQUAL,
  ND_GREATER,
  ND_GREATER_EQUAL,
  ND_SMALLER,
  ND_SMALLER_EQUAL,
  ND_ASSIGN,
  ND_VAR,
  ND_VAR_DEFINE,
  ND_ADDR,
  ND_DEREF,
  ND_RETURN,
  ND_IF,
  ND_IFELSE,
  ND_WHILE,
  ND_FOR,
  ND_BLOCK,
  ND_FUNCTION_CALL,
  ND_NUM,
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
};

struct NodeList {
  NodeList *next;
  Node *node;
};

typedef enum {
  TK_RESERVED,
  TK_RETURN,
  TK_SIZEOF,
  TK_IF,
  TK_ELSE,
  TK_WHILE,
  TK_FOR,
  TK_IDENT,
  TK_INT,
  TK_NUM,
  TK_EOF,
} TokenKind;

typedef struct Token Token;

struct Token{
  TokenKind kind;
  Token *next;
  int val;
  char *str;
  int len;
};


typedef struct Obj Obj;
typedef struct ObjList ObjList;

struct Obj {
  Type *type;
  char *name;
  int len;

  // for lvar
  int offset;

  // for function
  ObjList *arg_front;
  ObjList *arg_back;
  ObjList *locals;
  NodeList *code_front;
  NodeList *code_back;
  int arg_size;
};

struct ObjList {
  ObjList *next;
  Obj *obj;
};

extern const char variable_letters[];

extern char *user_input;
extern ObjList *globals;

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

bool consume(Token **rest,Token *token,char* op);
Token *consume_kind(Token **rest,Token *token,TokenKind kind);
void expect(Token **rest,Token *token,char* op);
Token *expect_kind(Token **rest,Token *token,TokenKind kind);
int expect_number(Token **rest,Token *token);
bool at_eof(Token *token);

bool is_alnum(char c);

Token *tokenize(char *p);

void debug_token(Token *token);

bool is_convertible(Type *a,Type *b);
int type_size(Type *a);
int offset_alignment(int start,int data_size,int alignment);

void program(Token *tok);

void codegen_all(FILE *output);



