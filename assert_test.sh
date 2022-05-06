#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./mycc "$input" > tmp.s
  gcc -o tmp tmp.s
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

echo OK
