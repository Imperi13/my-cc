CC=gcc
CFLAGS=-std=c11 -g -static
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

mycc: $(OBJS)
	$(CC) -o mycc $(OBJS) $(LDFLAGS)

test: mycc
	./assert_test.sh

external-test: mycc
	./external/test_cases.sh

clean:
	rm -f mycc *.o *~ tmp* supplement* donuts*

donuts: mycc
	./mycc ./sample/donuts.c > ./donuts.s
	gcc -o donuts ./donuts.s

franken: mycc
	./mycc file.c > file.s
	rm file.o
	gcc -c file.s
	rm file.s
	gcc -o mycc *.o

.PHONY: test external-test clean
