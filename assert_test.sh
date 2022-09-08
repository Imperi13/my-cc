#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./mycc ${input} -o tmp.s
  gcc -o tmp tmp.s util/ten.o util/add.o util/many_arg.o util/alloc4.o
  ./tmp
  actual="$?"

  echo "testcase: $2"

  if [ "$actual" = "$expected" ]; then
    cat "$input"
    echo "=> $actual"
  else
    cat "$input"
    echo "=> $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 "test/num1.c"
assert 42 "test/num2.c"

assert 41 "test/add1.c"

assert 34 "test/sisoku1.c"

assert 34 "test/sisoku2.c"

assert 2 "test/mod1.c"

assert 1 "test/ne1.c"
assert 0 "test/cmpl1.c"
assert 1 "test/cmpge1.c"

assert 10 "test/variable1.c"

assert 10 "test/variable2.c"

assert 45 "test/variable3.c"

assert 10 "test/return1.c"

assert 4 "test/if_stmt1.c"
assert 3 "test/if_stmt2.c"
assert 20 "test/if_stmt3.c"

assert 0 "test/while_stmt1.c"

assert 45 "test/while_stmt2.c"

assert 45 "test/for_stmt1.c"

assert 3 "test/block1.c"
assert 4 "test/block2.c"
assert 120 "test/block3.c"

assert 55 "test/function_call1.c"
assert 23 "test/function_call2.c"
assert 28 "test/function_call3.c"
assert 32 "test/function_call4.c"

assert 5 "test/function_definition1.c"
assert 25 "test/function_definition2.c"
assert 34 "test/function_definition3.c"

assert 3 "test/addr_deref1.c"
assert 3 "test/addr_deref2.c"
assert 3 "test/addr_deref3.c"

assert 12 "test/ptr_add1.c"

assert 4 "test/sizeof1.c"
assert 8 "test/sizeof2.c"

assert 10 "test/array1.c"
assert 3 "test/array2.c"
assert 34 "test/array3.c"
assert 34 "test/array4.c"
#assert 30 "test/array5.c"

assert 45 "test/add_assign1.c"
assert 15 "test/add_assign2.c"

assert 45 "test/post_increment1.c"
assert 20 "test/post_increment2.c"
assert 1 "test/post_increment3.c"
assert 9 "test/post_decrement1.c"
assert 10 "test/post_decrement2.c"

assert 45 "test/pre_increment1.c"
assert 20 "test/pre_increment2.c"
assert 2 "test/pre_increment3.c"

assert 0 "test/global_var1.c"
assert 10 "test/global_var2.c"
assert 30 "test/global_var3.c"

assert 8 "test/char1.c"
assert 40 "test/char2.c"

assert 0 "test/str_literal1.c"
assert 48 "test/str_literal2.c"

assert 10 "test/comment1.c"
assert 10 "test/comment2.c"

assert 20 "test/lvar_init1.c"

assert 9 "test/and1.c"
assert 6 "test/xor1.c"
assert 15 "test/or1.c"

assert 1 "test/conditional1.c"
assert 20 "test/conditional2.c"

assert 20 "test/comma1.c"

echo OK
