#!/bin/bash

compile="file.c main.c tokenize.c preprocess.c parse.c type.c analyze.c codegen.c"

rm -f ./build/*

gcc -c *.c
gcc -o ./build/mycc1 *.o

for filepath in ${compile}; do
  ./build/mycc1 ${filepath} > ${filepath%.c}.s;
  gcc -c ${filepath%.c}.s
  rm -f ${filepath%.c}.s
done

gcc -o ./build/mycc2 *.o
rm *.o


for filepath in ${compile}; do
  ./build/mycc1 ${filepath} > ./build/${filepath%.c}1.s;
  if [ $? -ne 0 ]; then { exit 1; }; fi

  echo "${filepath%.c}1.c is compiled"

  ./build/mycc2 ${filepath} > ./build/${filepath%.c}2.s;
  if [ $? -ne 0 ]; then { exit 1; }; fi

  echo "${filepath%.c}2.c is compiled"

  diff ./build/${filepath%.c}1.s ./build/${filepath%.c}2.s;
  if [ $? -ne 0 ]; then { echo "FAIL";exit 1; }; else echo "PASS"; fi
done


