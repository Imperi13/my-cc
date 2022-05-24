#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

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
  ND_LVAR,
  ND_LVAR_DEFINE,
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

typedef struct Type Type;

struct Type {
  enum {INT,PTR,ARRAY} ty;
  Type *ptr_to;
  size_t array_size;
};

typedef struct Node Node;
typedef struct StmtList StmtList;
typedef struct ExprList ExprList;

struct Node {
  NodeKind kind;
  Type *type;
  Node *lhs;
  Node *rhs;
  Node *expr;
  Node *init_expr;
  Node *update_expr;
  StmtList *stmt_front;
  StmtList *stmt_back;
  char *func_name;
  int func_name_len;
  ExprList *expr_front;
  ExprList *expr_back;
  int val;
  int offset;
};

struct StmtList {
  StmtList *next;
  Node *stmt;
};

struct ExprList {
  ExprList *next;
  Node *expr;
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

typedef struct LVar LVar;

struct LVar {
  LVar *next;
  Type *type;
  char *name;
  int len;
  int offset;
};

typedef struct ArgList ArgList;
typedef struct Function Function;

struct ArgList{
  ArgList *next;
  Type *type;
  LVar *lvar;
};

struct Function {
  Function *next;
  Type *return_type;
  ArgList *arg_front;
  ArgList *arg_back;
  StmtList *code_front;
  StmtList *code_back;
  LVar *locals;
  char *func_name;
  int func_name_len;
  int arg_count;
};

extern const char variable_letters[];

extern char *user_input;
extern Function *functions;

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

bool consume(Token **rest,Token *token,char* op);
Token *consume_kind(Token **rest,Token *token,TokenKind kind);
void expect(Token **rest,Token *token,char* op);
Token *expect_kind(Token **rest,Token *token,TokenKind kind);
int expect_number(Token **rest,Token *token);
bool at_eof(Token *token);

bool is_alnum(char c);

Token *new_token(TokenKind kind,Token *cur, char *str,int len);
Token *tokenize(char *p);

LVar *find_lvar(Token *tok);

void debug_token(Token *token);

Node *new_node(NodeKind kind, Node *lhs, Node *rhs,Type *type);
Node *new_node_num(int val);

bool is_convertible(Type *a,Type *b);
int type_size(Type *a);
int offset_alignment(int start,int data_size,int alignment);

void program(Token *tok);
Function *func_definition(Token **rest,Token *tok);
Node *stmt(Token **rest,Token *tok);
Node *expr(Token **rest,Token *tok);
Node *assign(Token **rest,Token *tok);
Node *equality(Token **rest,Token *tok);
Node *relational(Token **rest,Token *tok);
Node *add(Token **rest,Token *tok);
Node *mul(Token **rest,Token *tok);
Node *unary(Token **rest,Token *tok);
Node *postfix(Token **rest,Token *tok);
Node *primary(Token **rest,Token *tok);

void gen(Node *node);
void gen_function(Function *func);
void codegen_all(FILE *output);



