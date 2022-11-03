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
extern Register reg_xmm2;
extern Register reg_xmm3;
extern Register reg_xmm4;
extern Register reg_xmm5;
extern Register reg_xmm6;
extern Register reg_xmm7;

extern Register *call_register[6];
extern Register *call_SSE_register[8];

char get_size_suffix(Type *type);
char get_floating_point_suffix(Type *floating_type);
char *get_reg_alias(Register *reg, Type *type);
char *get_SSE_reg_alias(Register *reg);

// push/pop least 8bytes
void push_reg(FILE *codegen_output, Register *reg);
void pop_reg(FILE *codegen_output, Register *reg);

void mov_reg(FILE *codegen_output, Register *src, Register *dst, Type *type);

void mov_imm(FILE *codegen_output, Register *reg, Type *type,
             unsigned long long imm_val);
void reg_integer_cast(FILE *codegen_output, Register *reg, Type *src_type,
                      Type *dst_type);
void reg_arithmetic_cast(FILE *codegen_output, Register *src_reg,
                         Register *dst_reg, Type *src_type, Type *dst_type);

// arithmetic operator
// scalar type : lhs - rax, rhs - rdi
// floating point type : lhs - xmm0, rhs - xmm1
// result save to rax
void add_reg(FILE *codegen_output, Type *type);
void sub_reg(FILE *codegen_output, Type *type);
void mul_reg(FILE *codegen_output, Type *type);
void div_reg(FILE *codegen_output, Type *type);
void mod_reg(FILE *codegen_output, Type *type);
void lshift_reg(FILE *codegen_output, Type *lhs_type, Type *rhs_type);
void rshift_reg(FILE *codegen_output, Type *lhs_type, Type *rhs_type);

// argument classification
typedef enum ArgClass {
  ARG_NOCLASS,
  ARG_INTEGER,
  ARG_SSE,
  ARG_MEMORY,
} ArgClass;

ArgClass classify_argument(Type *argtype);
