#!/bin/bash

compile="file.c main.c tokenize.c preprocess.c parse.c type.c analyze.c codegen.c error.c str_dict.c"
cc=/usr/local/musl/bin/musl-gcc
cc_option="--std=gnu11 -g --static"

rm -f ./build/*

${cc} ${cc_option} -c *.c
${cc} ${cc_option} -o ./build/mycc1 *.o

for filepath in ${compile}; do
  ./build/mycc1 ${filepath} -o ${filepath%.c}.s;
  ${cc} ${cc_option} -c ${filepath%.c}.s
  rm -f ${filepath%.c}.s
done

${cc} ${cc_option} -o ./build/mycc2 *.o
rm *.o


for filepath in ${compile}; do
  ./build/mycc1 ${filepath} -o ./build/${filepath%.c}1.s;
  if [ $? -ne 0 ]; then { exit 1; }; fi

  echo "${filepath%.c}1.c is compiled"

  ./build/mycc2 ${filepath} -o ./build/${filepath%.c}2.s;
  if [ $? -ne 0 ]; then { exit 1; }; fi

  echo "${filepath%.c}2.c is compiled"

  diff ./build/${filepath%.c}1.s ./build/${filepath%.c}2.s;
  if [ $? -ne 0 ]; then { echo "FAIL";exit 1; }; else echo "PASS"; fi
done


