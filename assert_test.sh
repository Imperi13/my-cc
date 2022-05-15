#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./mycc "$input" > tmp.s
  gcc -o tmp tmp.s util/ten.o
  ./tmp
  actual="$?"

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

echo OK
