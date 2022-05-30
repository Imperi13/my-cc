CC=gcc
CFLAGS=-std=c11 -g -static
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

mycc: $(OBJS)
	$(CC) -o mycc $(OBJS) $(LDFLAGS)

$(OBJS): mycc.h

test: mycc
	./assert_test.sh

external-test: mycc
	./external/test_cases.sh

clean:
	rm -f mycc *.o *~ tmp*

.PHONY: test external-test clean
