#CC=/usr/local/musl/bin/musl-gcc
CC=gcc
CFLAGS=-std=gnu11 -g -static -Wall -Wextra
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

mycc: $(OBJS)
	$(CC) -o mycc $(OBJS) $(LDFLAGS)

test: mycc
	./assert_test.sh

external-test: mycc
	MYCC=./mycc ./external/test_cases.sh

clean:
	rm -f mycc mycc2 *.o *~ tmp* supplement* donuts*

donuts: mycc
	./mycc -S ./sample/donuts.c -o ./donuts.s
	$(CC) -o donuts ./donuts.s

franken:
	./franken_test.sh

mycc2: franken
	cp ./build/mycc2 ./mycc2

external-test-stage2: mycc2
	MYCC=./mycc2 ./external/test_cases.sh	


.PHONY: franken test external-test clean
