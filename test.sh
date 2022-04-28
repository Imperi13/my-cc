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

assert 0 0
assert 42 42

assert 41 " 12 + 34 - 5"

assert 34 " 2 *  (11 + 2*3 )"

assert 34 " -2 * ( 5 -2 *11)"

echo OK
