#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>

typedef enum {
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_NUM,
} NodeKind;

typedef struct Node Node;

struct Node {
  NodeKind kind;
  Node *lhs;
  Node *rhs;
  int val;
};

typedef enum {
  TK_RESERVED,
  TK_NUM,
  TK_EOF,
} TokenKind;

typedef struct Token Token;

struct Token{
  TokenKind kind;
  Token *next;
  int val;
  char *str;
};

extern Token *token;
extern char *user_input;

Node *expr();
Node *mul();
Node *unary();
Node *primary();

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

bool consume(char op);
void expect(char op);
int expect_number();
bool at_eof();

Node *new_node(NodeKind kind, Node *lhs, Node *rhs);
Node *new_node_num(int val);

Node *expr();
Node *mul();
Node *unary();
Node *primary();

Token *new_token(TokenKind kind,Token *cur, char *str);
Token *tokenize(char *p);

void gen(Node *node);




