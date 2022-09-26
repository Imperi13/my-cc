#pragma once

#include <stdio.h>

typedef struct Register Register;

#include "type.h"

extern Register reg_rax;
extern Register reg_rdi;
extern Register reg_rsi;
extern Register reg_rdx;
extern Register reg_rcx;
extern Register reg_r8;
extern Register reg_r9;

extern Register *call_register[6];

char get_size_suffix(Type *type);
char *get_reg_alias(Register *reg, Type *type);

void push_reg(FILE *codegen_output, Register *reg, Type *type);
void pop_reg(FILE *codegen_output, Register *reg, Type *type);
void mov_reg(FILE *codegen_output, Register *src, Register *dst, Type *type);

void mov_imm(FILE *codegen_output, Register *reg, Type *type, long imm_val);

// arithmetic operator
// lhs: rax, rhs:rdi
// result save to rax
void div_reg(FILE *codegen_output, Type *type);
void mod_reg(FILE *codegen_output, Type *type);
