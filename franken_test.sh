#!/bin/bash

compile="file.c"

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

./build/mycc1 file.c > ./build/file1.s
./build/mycc2 file.c > ./build/file2.s

diff ./build/file1.s ./build/file2.s

if [ $? -ne 0 ]; then { echo "FAIL";exit 1; }; else echo "PASS"; fi

