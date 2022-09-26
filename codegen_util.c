
#include <stdio.h>

#include "codegen_util.h"
#include "error.h"
#include "type.h"

Register reg_rax = {.alias = {"%al", "%ax", "%eax", "%rax"}};
Register reg_rdi = {.alias = {"%dil", "%di", "%edi", "%rdi"}};
Register reg_rsi = {.alias = {"%sil", "%si", "%esi", "%rsi"}};
Register reg_rdx = {.alias = {"%dl", "%dx", "%edx", "%rdx"}};
Register reg_rcx = {.alias = {"%cl", "%cx", "%ecx", "%rcx"}};
Register reg_r8 = {.alias = {"%r8b", "%r8w", "%r8d", "%r8"}};
Register reg_r9 = {.alias = {"%r9b", "%r9w", "%r9d", "%r9"}};

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
  } else
    not_implemented(__func__);
}

void pop_reg(FILE *codegen_output, Register *reg, Type *type) {
  if (is_scalar(type)) {
    fprintf(codegen_output, "  popq %s\n", reg->alias[3]);
  } else
    not_implemented(__func__);
}

void mov_reg(FILE *codegen_output, Register *dst, Register *src, Type *type) {
  assert(is_scalar(type), "not scalar type");

  fprintf(codegen_output, "  mov%c %s, %s\n", get_size_suffix(type),
          get_reg_alias(src, type), get_reg_alias(dst, type));
}

void mov_imm(FILE *codegen_output, Register *reg, Type *type, long imm_val) {
  assert(is_scalar(type), "not scalar type");

  fprintf(codegen_output, "  mov%c $%ld, %s\n", get_size_suffix(type), imm_val,
          get_reg_alias(reg, type));
}
