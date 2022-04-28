.intel_syntax noprefix
.globl main
main:
  push 100
  push 50
  push 2
  pop rdi
  pop rax
  imul rax,rdi
  push rax
  pop rdi
  pop rax
  cmp rdi,rax
  setle al
  movzb rax,al
  push rax
  pop rax
  ret
