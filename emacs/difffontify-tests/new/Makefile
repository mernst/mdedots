CC = gcc
CFLAGS = -O2 -Wall

all: foo

foo: foo.c
	$(CC) $(CFLAGS) -o foo foo.c -lm -lpthread -ldl -lrt -lz -lssl -lcrypto -lcurl -lxml2 -lyaml -lsqlite3 -lpng
