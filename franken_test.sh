#!/bin/bash

compile="*.c"
#cc=/usr/local/musl/bin/musl-gcc
cc=gcc
cc_option="--std=gnu11 -g --static -Wall -Wextra"

echo ${compile}

rm -f ./build/*

${cc} ${cc_option} -o ./build/mycc1 ${compile}

./build/mycc1 -o ./build/mycc2 ${compile}

for filepath in ${compile}; do
  ./build/mycc1 -S ${filepath} -o ./build/${filepath%.c}1.s;
  if [ $? -ne 0 ]; then { exit 1; }; fi

  echo "${filepath%.c}1.c is compiled"

  ./build/mycc2 -S ${filepath} -o ./build/${filepath%.c}2.s;
  if [ $? -ne 0 ]; then { exit 1; }; fi

  echo "${filepath%.c}2.c is compiled"

  diff ./build/${filepath%.c}1.s ./build/${filepath%.c}2.s;
  if [ $? -ne 0 ]; then { echo "FAIL";exit 1; }; else echo "PASS"; fi
done


