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

extern Register reg_xmm0;
extern Register reg_xmm1;

extern Register *call_register[6];

char get_size_suffix(Type *type);
char get_floating_point_suffix(Type *floating_type);
char *get_reg_alias(Register *reg, Type *type);

void push_reg(FILE *codegen_output, Register *reg, Type *type);
void pop_reg(FILE *codegen_output, Register *reg, Type *type);
void mov_reg(FILE *codegen_output, Register *src, Register *dst, Type *type);

void mov_imm(FILE *codegen_output, Register *reg, Type *type,
             unsigned long long imm_val);
void reg_integer_cast(FILE *codegen_output, Register *reg, Type *src_type,
                      Type *dst_type);
void reg_arithmetic_cast(FILE *codegen_output, Register *src_reg,
                         Register *dst_reg, Type *src_type, Type *dst_type);

// arithmetic operator
// lhs: rax, rhs:rdi
// result save to rax
void mul_reg(FILE *codegen_output, Type *type);
void div_reg(FILE *codegen_output, Type *type);
void mod_reg(FILE *codegen_output, Type *type);
void lshift_reg(FILE *codegen_output, Type *lhs_type, Type *rhs_type);
void rshift_reg(FILE *codegen_output, Type *lhs_type, Type *rhs_type);
