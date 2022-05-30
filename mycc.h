#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

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
  TK_CHAR,
  TK_NUM,
  TK_STR,
  TK_EOF,
} TokenKind;

typedef struct Token Token;
typedef struct StrLiteral StrLiteral;

struct Token{
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

typedef enum{
  INT,
  CHAR,
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
  size_t arg_size;
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
  ND_BIT_AND,
  ND_BIT_XOR,
  ND_BIT_OR,
  ND_MOD,
  ND_EQUAL,
  ND_NOT_EQUAL,
  ND_GREATER,
  ND_GREATER_EQUAL,
  ND_SMALLER,
  ND_SMALLER_EQUAL,
  ND_ASSIGN,
  ND_VAR,
  ND_STR,
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
};

struct NodeList {
  NodeList *next;
  Node *node;
};

typedef struct Obj Obj;
typedef struct ObjList ObjList;

struct Obj {
  Type *type;
  char *name;
  int len;

  // for lvar
  int offset;
  Node *init_var;

  // for function
  ObjList *arg_front;
  ObjList *arg_back;
  ObjList *locals;
  Node *code;
  int arg_size;
  bool is_defined;
};

struct ObjList {
  ObjList *next;
  Obj *obj;
};

extern const char variable_letters[];
extern Token *dummy_token;
extern Type *type_int;
extern Type *type_char;

extern char *filename;
extern char *user_input;
extern StrLiteral *str_literals;
extern ObjList *globals;
extern Obj *now_function;

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

bool equal(Token *token,char *op);
bool equal_kind(Token *token,TokenKind kind);
bool consume(Token **rest,Token *token,char* op);
Token *consume_kind(Token **rest,Token *token,TokenKind kind);
void expect(Token **rest,Token *token,char* op);
Token *expect_kind(Token **rest,Token *token,TokenKind kind);
int expect_number(Token **rest,Token *token);
bool at_eof(Token *token);

bool is_alnum(char c);

Token *tokenize(char *p);

void debug_token(Token *token);

Obj *parse_global_decl(Token **rest,Token *tok);
Obj *parse_local_decl(Token **rest,Token *tok);
Type *newtype_ptr(Type *base);
bool is_numeric(Type *a);
bool is_same_type(Type *a,Type *b);
bool is_convertible(Type *a,Type *b);
int type_size(Type *a);
int type_alignment(Type *a);
int offset_alignment(int start,int data_size,int alignment);

Obj *find_obj(ObjList *list,char *str,int len);
Obj *find_lvar(char *str,int len);
void program(Token *tok);
Node *assign(Token **rest,Token *tok);

void codegen_all(FILE *output);



