
#include <stdio.h>

#include "codegen_util.h"
#include "error.h"
#include "type.h"

Register reg_rax = {.alias = {"al", "ax", "eax", "rax"}};
Register reg_rdi = {.alias = {"dil", "di", "edi", "rdi"}};
Register reg_rsi = {.alias = {"sil", "si", "esi", "rsi"}};
Register reg_rdx = {.alias = {"dl", "dx", "edx", "rdx"}};
Register reg_rcx = {.alias = {"cl", "cx", "ecx", "rcx"}};
Register reg_r8 = {.alias = {"r8b", "r8w", "r8d", "r8"}};
Register reg_r9 = {.alias = {"r9b", "r9w", "r9d", "r9"}};

void push_reg(FILE *codegen_output, Register *reg, Type *type) {
  if (is_scalar(type)) {
    if (type_size(type) == 1)
      fprintf(codegen_output, "  push %s\n", reg->alias[0]);
    else if (type_size(type) == 2)
      fprintf(codegen_output, "  push %s\n", reg->alias[1]);
    else if (type_size(type) == 4)
      fprintf(codegen_output, "  push %s\n", reg->alias[2]);
    else if (type_size(type) == 8)
      fprintf(codegen_output, "  push %s\n", reg->alias[3]);
    else
      error("invalid type");
  } else {
    fprintf(codegen_output, "  push %s\n", reg->alias[3]);
  }
}

void pop_reg(FILE *codegen_output, Register *reg, Type *type) {
  if (is_scalar(type)) {
    if (type_size(type) == 1)
      fprintf(codegen_output, "  pop %s\n", reg->alias[0]);
    else if (type_size(type) == 2)
      fprintf(codegen_output, "  pop %s\n", reg->alias[1]);
    else if (type_size(type) == 4)
      fprintf(codegen_output, "  pop %s\n", reg->alias[2]);
    else if (type_size(type) == 8)
      fprintf(codegen_output, "  pop %s\n", reg->alias[3]);
    else
      error("invalid type");
  } else {
    fprintf(codegen_output, "  pop %s\n", reg->alias[3]);
  }
}
