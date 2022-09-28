
#include <stdio.h>

#include "codegen_util.h"
#include "error.h"
#include "type.h"

struct Register {
  char *alias[4];
};

Register reg_rax = {.alias = {"%al", "%ax", "%eax", "%rax"}};
Register reg_rdi = {.alias = {"%dil", "%di", "%edi", "%rdi"}};
Register reg_rsi = {.alias = {"%sil", "%si", "%esi", "%rsi"}};
Register reg_rdx = {.alias = {"%dl", "%dx", "%edx", "%rdx"}};
Register reg_rcx = {.alias = {"%cl", "%cx", "%ecx", "%rcx"}};
Register reg_r8 = {.alias = {"%r8b", "%r8w", "%r8d", "%r8"}};
Register reg_r9 = {.alias = {"%r9b", "%r9w", "%r9d", "%r9"}};

Register *call_register[6] = {&reg_rdi, &reg_rsi, &reg_rdx,
                              &reg_rcx, &reg_r8,  &reg_r9};

char get_size_suffix(Type *type) {
  assert(is_scalar(type), "not scalar type");
  if (type_size(type) == 1)
    return 'b';
  else if (type_size(type) == 2)
    return 'w';
  else if (type_size(type) == 4)
    return 'l';
  else if (type_size(type) == 8)
    return 'q';
  else
    error("invalid type");
}

char *get_reg_alias(Register *reg, Type *type) {
  assert(is_scalar(type), "not scalar type");
  if (type_size(type) == 1)
    return reg->alias[0];
  else if (type_size(type) == 2)
    return reg->alias[1];
  else if (type_size(type) == 4)
    return reg->alias[2];
  else if (type_size(type) == 8)
    return reg->alias[3];
  else
    error("invalid type");
}

void push_reg(FILE *codegen_output, Register *reg, Type *type) {
  if (is_scalar(type)) {
    fprintf(codegen_output, "  pushq %s\n", reg->alias[3]);
  } else {
    fprintf(codegen_output, "  pushq %s\n", reg->alias[3]);
  }
}

void pop_reg(FILE *codegen_output, Register *reg, Type *type) {
  if (is_scalar(type)) {
    fprintf(codegen_output, "  popq %s\n", reg->alias[3]);
  } else {
    fprintf(codegen_output, "  popq %s\n", reg->alias[3]);
  }
}

void mov_reg(FILE *codegen_output, Register *src, Register *dst, Type *type) {
  assert(is_scalar(type), "not scalar type");

  fprintf(codegen_output, "  mov%c %s, %s\n", get_size_suffix(type),
          get_reg_alias(src, type), get_reg_alias(dst, type));
}

void mov_imm(FILE *codegen_output, Register *reg, Type *type, long imm_val) {
  assert(is_scalar(type), "not scalar type");

  fprintf(codegen_output, "  mov%c $%ld, %s\n", get_size_suffix(type), imm_val,
          get_reg_alias(reg, type));
}

void reg_integer_cast(FILE *codegen_output, Register *reg, Type *src_type,
                      Type *dst_type) {
  assert(is_integer(src_type) && is_integer(dst_type), "not integer type");

  if (dst_type->kind == BOOL) {
    fprintf(codegen_output, "  cmp%c $0, %s\n", get_size_suffix(src_type),
            get_reg_alias(reg, src_type));
    fprintf(codegen_output, "  setne %s\n", get_reg_alias(reg, dst_type));
    return;
  }

  if (type_size(dst_type) <= type_size(src_type))
    return;

  if (src_type->is_unsigned)
    fprintf(codegen_output, "  movz%c%c %s, %s\n", get_size_suffix(src_type),
            get_size_suffix(dst_type), get_reg_alias(reg, src_type),
            get_reg_alias(reg, dst_type));
  else
    fprintf(codegen_output, "  movs%c%c %s, %s\n", get_size_suffix(src_type),
            get_size_suffix(dst_type), get_reg_alias(reg, src_type),
            get_reg_alias(reg, dst_type));
}

void div_reg(FILE *codegen_output, Type *type) {
  assert(is_arithmetic(type), "not arighmetic type");

  if (is_integer(type)) {
    if (type->is_unsigned) {
      if (type_size(type) == 4) {
        fprintf(codegen_output, "  movl $0, %%edx\n");
        fprintf(codegen_output, "  divl %%edi\n");
      } else if (type_size(type) == 8) {
        fprintf(codegen_output, "  movq $0, %%rdx\n");
        fprintf(codegen_output, "  divq %%rdi\n");
      } else
        not_implemented(__func__);
    } else {
      if (type_size(type) == 4) {
        fprintf(codegen_output, "  cltd\n");
        fprintf(codegen_output, "  idivl %%edi\n");
      } else if (type_size(type) == 8) {
        fprintf(codegen_output, "  cqto\n");
        fprintf(codegen_output, "  idivq %%rdi\n");
      } else
        not_implemented(__func__);
    }
  } else
    not_implemented(__func__);
}

void mod_reg(FILE *codegen_output, Type *type) {
  assert(is_integer(type), "not integer type");

  if (type->is_unsigned) {
    if (type_size(type) == 4) {
      fprintf(codegen_output, "  movl $0, %%edx\n");
      fprintf(codegen_output, "  divl %%edi\n");
      fprintf(codegen_output, "  movl %%edx, %%eax\n");
    } else if (type_size(type) == 8) {
      fprintf(codegen_output, "  movq $0, %%rdx\n");
      fprintf(codegen_output, "  divq %%rdi\n");
      fprintf(codegen_output, "  movq %%rdx, %%rax\n");
    } else
      not_implemented(__func__);
  } else {
    if (type_size(type) == 4) {
      fprintf(codegen_output, "  cltd\n");
      fprintf(codegen_output, "  idivl %%edi\n");
      fprintf(codegen_output, "  movl %%edx, %%eax\n");
    } else if (type_size(type) == 8) {
      fprintf(codegen_output, "  cqto\n");
      fprintf(codegen_output, "  idivq %%rdi\n");
      fprintf(codegen_output, "  movq %%rdx, %%rax\n");
    } else
      not_implemented(__func__);
  }
}
