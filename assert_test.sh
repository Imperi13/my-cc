#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./mycc "$input" > tmp.s
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

assert 0 "test/num1.in"
assert 42 "test/num2.in"

assert 41 "test/add1.in"

assert 34 "test/sisoku1.in"

assert 34 "test/sisoku2.in"

assert 2 "test/mod1.in"

assert 1 "test/ne1.in"
assert 0 "test/cmpl1.in"
assert 1 "test/cmpge1.in"

assert 10 "test/variable1.in"

assert 10 "test/variable2.in"

assert 45 "test/variable3.in"

assert 10 "test/return1.in"

assert 4 "test/if_stmt1.in"
assert 3 "test/if_stmt2.in"
assert 20 "test/if_stmt3.in"

assert 0 "test/while_stmt1.in"

assert 45 "test/while_stmt2.in"

assert 45 "test/for_stmt1.in"

assert 3 "test/block1.in"
assert 4 "test/block2.in"
assert 120 "test/block3.in"

assert 55 "test/function_call1.in"
assert 23 "test/function_call2.in"
assert 28 "test/function_call3.in"
assert 32 "test/function_call4.in"

assert 5 "test/function_definition1.in"
assert 25 "test/function_definition2.in"
assert 34 "test/function_definition3.in"

assert 3 "test/addr_deref1.in"
assert 3 "test/addr_deref2.in"
assert 3 "test/addr_deref3.in"

assert 12 "test/ptr_add1.in"

assert 4 "test/sizeof1.in"
assert 8 "test/sizeof2.in"

assert 10 "test/array1.in"
assert 3 "test/array2.in"
assert 34 "test/array3.in"
assert 34 "test/array4.in"
#assert 30 "test/array5.in"

assert 45 "test/add_assign1.in"
assert 15 "test/add_assign2.in"

assert 45 "test/post_increment1.in"
assert 20 "test/post_increment2.in"
assert 1 "test/post_increment3.in"
assert 9 "test/post_decrement1.in"
assert 10 "test/post_decrement2.in"

assert 45 "test/pre_increment1.in"
assert 20 "test/pre_increment2.in"
assert 2 "test/pre_increment3.in"

assert 0 "test/global_var1.in"
assert 10 "test/global_var2.in"
assert 30 "test/global_var3.in"

assert 8 "test/char1.in"
assert 40 "test/char2.in"

assert 0 "test/str_literal1.in"
assert 48 "test/str_literal2.in"

assert 10 "test/comment1.in"
assert 10 "test/comment2.in"

assert 20 "test/lvar_init1.in"

assert 9 "test/and1.in"
assert 6 "test/xor1.in"
assert 15 "test/or1.in"

assert 1 "test/conditional1.in"
assert 20 "test/conditional2.in"

assert 20 "test/comma1.in"

echo OK
