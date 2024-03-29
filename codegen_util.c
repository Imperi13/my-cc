
#include <stdio.h>

#include "codegen_util.h"
#include "error.h"
#include "type.h"

struct Register {
  char *alias[4];
  bool is_SSE;
};

Register reg_rax = {.alias = {"%al", "%ax", "%eax", "%rax"}};
Register reg_rdi = {.alias = {"%dil", "%di", "%edi", "%rdi"}};
Register reg_rsi = {.alias = {"%sil", "%si", "%esi", "%rsi"}};
Register reg_rdx = {.alias = {"%dl", "%dx", "%edx", "%rdx"}};
Register reg_rcx = {.alias = {"%cl", "%cx", "%ecx", "%rcx"}};
Register reg_r8 = {.alias = {"%r8b", "%r8w", "%r8d", "%r8"}};
Register reg_r9 = {.alias = {"%r9b", "%r9w", "%r9d", "%r9"}};

Register reg_xmm0 = {.alias = {"%xmm0", "", "", ""}, .is_SSE = true};
Register reg_xmm1 = {.alias = {"%xmm1", "", "", ""}, .is_SSE = true};
Register reg_xmm2 = {.alias = {"%xmm2", "", "", ""}, .is_SSE = true};
Register reg_xmm3 = {.alias = {"%xmm3", "", "", ""}, .is_SSE = true};
Register reg_xmm4 = {.alias = {"%xmm4", "", "", ""}, .is_SSE = true};
Register reg_xmm5 = {.alias = {"%xmm5", "", "", ""}, .is_SSE = true};
Register reg_xmm6 = {.alias = {"%xmm6", "", "", ""}, .is_SSE = true};
Register reg_xmm7 = {.alias = {"%xmm7", "", "", ""}, .is_SSE = true};

Register *call_register[6] = {&reg_rdi, &reg_rsi, &reg_rdx,
                              &reg_rcx, &reg_r8,  &reg_r9};

Register *call_SSE_register[8] = {&reg_xmm0, &reg_xmm1, &reg_xmm2, &reg_xmm3,
                                  &reg_xmm4, &reg_xmm5, &reg_xmm6, &reg_xmm7};

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

char get_floating_point_suffix(Type *floating_type) {
  assert(is_floating_point(floating_type), "not floating type");
  if (floating_type->kind == FLOAT)
    return 's';
  else if (floating_type->kind == DOUBLE)
    return 'd';
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

char *get_SSE_reg_alias(Register *reg) {
  assert(reg->is_SSE, "not SSE reg");
  return reg->alias[0];
}

void push_reg(FILE *codegen_output, Register *reg) {
  if (reg->is_SSE) {
    fprintf(codegen_output, "  subq $8, %%rsp\n");
    fprintf(codegen_output, "  movsd %s, 0(%%rsp)\n", reg->alias[0]);
  } else {
    fprintf(codegen_output, "  pushq %s\n", reg->alias[3]);
  }
}

void pop_reg(FILE *codegen_output, Register *reg) {
  if (reg->is_SSE) {
    fprintf(codegen_output, "  movsd 0(%%rsp), %s\n", reg->alias[0]);
    fprintf(codegen_output, "  addq $8, %%rsp\n");
  } else {
    fprintf(codegen_output, "  popq %s\n", reg->alias[3]);
  }
}

void mov_reg(FILE *codegen_output, Register *src, Register *dst, Type *type) {
  assert(src->is_SSE ^ is_scalar(type), "invalid register");
  assert(dst->is_SSE ^ is_scalar(type), "invalid register");

  if (is_scalar(type))
    fprintf(codegen_output, "  mov%c %s, %s\n", get_size_suffix(type),
            get_reg_alias(src, type), get_reg_alias(dst, type));
  else if (is_floating_point(type))
    fprintf(codegen_output, "  movs%c %s, %s\n",
            get_floating_point_suffix(type), src->alias[0], dst->alias[0]);
  else
    error("mov_reg");
}

void mov_imm(FILE *codegen_output, Register *reg, Type *type,
             unsigned long long imm_val) {
  assert(is_scalar(type), "not scalar type");

  fprintf(codegen_output, "  mov%c $%llu, %s\n", get_size_suffix(type), imm_val,
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

void reg_arithmetic_cast(FILE *codegen_output, Register *src_reg,
                         Register *dst_reg, Type *src_type, Type *dst_type) {
  assert(is_arithmetic(src_type) && is_arithmetic(dst_type),
         "not arithmetic type");
  assert(src_reg->is_SSE ^ is_integer(src_type), "invalid register");
  assert(dst_reg->is_SSE ^ is_integer(dst_type), "invalid register");

  if (is_same_type(src_type, dst_type))
    return;

  if (dst_type->kind == BOOL) {
    if (is_scalar(src_type)) {
      fprintf(codegen_output, "  cmp%c $0, %s\n", get_size_suffix(src_type),
              get_reg_alias(src_reg, src_type));
      fprintf(codegen_output, "  setne %s\n", get_reg_alias(dst_reg, dst_type));
    } else
      not_implemented(__func__);
    return;
  }

  if (is_integer(src_type) && is_integer(dst_type)) {

    if (type_size(dst_type) <= type_size(src_type))
      return;

    if (src_type->is_unsigned)
      fprintf(codegen_output, "  movz%c%c %s, %s\n", get_size_suffix(src_type),
              get_size_suffix(dst_type), get_reg_alias(src_reg, src_type),
              get_reg_alias(dst_reg, dst_type));
    else
      fprintf(codegen_output, "  movs%c%c %s, %s\n", get_size_suffix(src_type),
              get_size_suffix(dst_type), get_reg_alias(src_reg, src_type),
              get_reg_alias(dst_reg, dst_type));

  } else if (is_floating_point(src_type) && is_integer(dst_type)) {
    fprintf(codegen_output, "  cvtts%c2si%c %s, %s\n",
            get_floating_point_suffix(src_type), get_size_suffix(dst_type),
            src_reg->alias[0], get_reg_alias(dst_reg, dst_type));

  } else if (is_integer(src_type) && is_floating_point(dst_type)) {
    fprintf(codegen_output, " cvtsi2s%c%c %s, %s\n",
            get_floating_point_suffix(dst_type), get_size_suffix(src_type),
            get_reg_alias(src_reg, src_type), dst_reg->alias[0]);

  } else if (is_floating_point(src_type) && is_floating_point(dst_type)) {
    fprintf(codegen_output, "  cvts%c2s%c %s, %s\n",
            get_floating_point_suffix(src_type),
            get_floating_point_suffix(dst_type), src_reg->alias[0],
            dst_reg->alias[0]);
  } else
    error("invalid type pair");
}

void add_reg(FILE *codegen_output, Type *type) {
  if (is_scalar(type)) {
    fprintf(codegen_output, "  add%c %s, %s\n", get_size_suffix(type),
            get_reg_alias(&reg_rdi, type), get_reg_alias(&reg_rax, type));
  } else {
    fprintf(codegen_output, "  adds%c %%xmm1, %%xmm0\n",
            get_floating_point_suffix(type));
  }
}

void sub_reg(FILE *codegen_output, Type *type) {
  if (is_scalar(type)) {
    fprintf(codegen_output, "  sub%c %s, %s\n", get_size_suffix(type),
            get_reg_alias(&reg_rdi, type), get_reg_alias(&reg_rax, type));
  } else {
    fprintf(codegen_output, "  subs%c %%xmm1, %%xmm0\n",
            get_floating_point_suffix(type));
  }
}

void mul_reg(FILE *codegen_output, Type *type) {
  if (is_floating_point(type)) {
    fprintf(codegen_output, "  muls%c %%xmm1, %%xmm0\n",
            get_floating_point_suffix(type));
  } else if (type->is_unsigned) {
    fprintf(codegen_output, "  mul%c %s\n", get_size_suffix(type),
            get_reg_alias(&reg_rdi, type));
  } else {
    fprintf(codegen_output, "  imul%c %s, %s\n", get_size_suffix(type),
            get_reg_alias(&reg_rdi, type), get_reg_alias(&reg_rax, type));
  }
}

void div_reg(FILE *codegen_output, Type *type) {
  assert(is_arithmetic(type), "not arighmetic type");

  if (is_floating_point(type)) {
    fprintf(codegen_output, "  divs%c %%xmm1, %%xmm0\n",
            get_floating_point_suffix(type));
  } else if (is_integer(type)) {
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

void lshift_reg(FILE *codegen_output, Type *lhs_type, Type *rhs_type) {
  mov_reg(codegen_output, &reg_rdi, &reg_rcx, rhs_type);

  if (lhs_type->is_unsigned)
    fprintf(codegen_output, "  shl%c %%cl, %s\n", get_size_suffix(lhs_type),
            get_reg_alias(&reg_rax, lhs_type));
  else
    fprintf(codegen_output, "  sal%c %%cl, %s\n", get_size_suffix(lhs_type),
            get_reg_alias(&reg_rax, lhs_type));
}

void rshift_reg(FILE *codegen_output, Type *lhs_type, Type *rhs_type) {
  mov_reg(codegen_output, &reg_rdi, &reg_rcx, rhs_type);

  if (lhs_type->is_unsigned)
    fprintf(codegen_output, "  shr%c %%cl, %s\n", get_size_suffix(lhs_type),
            get_reg_alias(&reg_rax, lhs_type));
  else
    fprintf(codegen_output, "  sar%c %%cl, %s\n", get_size_suffix(lhs_type),
            get_reg_alias(&reg_rax, lhs_type));
}

// argument classification

ArgClass classify_argument(Type *argtype) {
  if (is_scalar(argtype))
    return ARG_INTEGER;

  if (is_floating_point(argtype))
    return ARG_SSE;

  not_implemented(__func__);
}
