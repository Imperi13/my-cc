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
  ND_EQUAL,
  ND_NOT_EQUAL,
  ND_GREATER,
  ND_GREATER_EQUAL,
  ND_SMALLER,
  ND_SMALLER_EQUAL,
  ND_ASSIGN,
  ND_LVAR,
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
typedef struct StmtList StmtList;

struct Node {
  NodeKind kind;
  Node *lhs;
  Node *rhs;
  Node *expr;
  Node *init_expr;
  Node *update_expr;
  StmtList *front;
  StmtList *back;
  char *func_name;
  int func_name_len;
  int val;
  int offset;
};

struct StmtList {
  StmtList *next;
  Node *stmt;
};

typedef enum {
  TK_RESERVED,
  TK_RETURN,
  TK_IF,
  TK_ELSE,
  TK_WHILE,
  TK_FOR,
  TK_IDENT,
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
  char *name;
  int len;
  int offset;
};

extern const char variable_letters[];

extern Token *token;
extern char *user_input;
//extern Node *code[100];
extern StmtList* code_front;
extern StmtList* code_back;
extern LVar *locals;

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

bool consume(char* op);
Token *consume_kind();
void expect(char* op);
int expect_number();
bool at_eof();

bool is_alnum(char c);

Token *new_token(TokenKind kind,Token *cur, char *str,int len);
Token *tokenize(char *p);

LVar *find_lvar(Token *tok);

void debug_token();

Node *new_node(NodeKind kind, Node *lhs, Node *rhs);
Node *new_node_num(int val);

void program();
Node *stmt();
Node *expr();
Node *assign();
Node *equality();
Node *relational();
Node *add();
Node *mul();
Node *unary();
Node *primary();

void gen(Node *node);
void codegen_all(FILE *output);



