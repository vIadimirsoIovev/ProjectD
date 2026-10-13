CC ?= cc
CFLAGS ?= -Wall -Wextra -std=c99

all: tokenizer parser

tokenizer: tokenizer.c
	$(CC) $(CFLAGS) -o $@ $<

parser: parser.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f tokenizer parser tokens

.PHONY: all clean
