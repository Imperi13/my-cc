#pragma once

typedef struct Register Register;
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
