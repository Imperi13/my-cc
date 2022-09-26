#pragma once

#include <stdio.h>

typedef struct Register Register;

#include "type.h"

struct Register {
  char *alias[4];
};

extern Register reg_rax;
extern Register reg_rdi;
extern Register reg_rsi;
extern Register reg_rdx;
extern Register reg_rcx;
extern Register reg_r8;
extern Register reg_r9;

char get_size_suffix(Type *type);
char *get_reg_alias(Register *reg, Type *type);

void push_reg(FILE *codegen_output, Register *reg, Type *type);
void pop_reg(FILE *codegen_output, Register *reg, Type *type);
void mov_reg(FILE *codegen_output, Register *dst, Register *src, Type *type);

void mov_imm(FILE *codegen_output, Register *reg, Type *type, long imm_val);
