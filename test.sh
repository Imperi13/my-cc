#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./mycc "$input" > tmp.s
  gcc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 "0;"
assert 42 "42;"

assert 41 " 12 + 34 - 5;"

assert 34 " 2 *  (11 + 2*3 );"

assert 34 " -2 * ( 5 -2 *11);"

assert 1 "10!=20;"
assert 0 "100 < 20 + 30;"
assert 1 "100 >= 50 * 2 ;"

assert 10 "a=2;b=20;b/a;"

echo OK
