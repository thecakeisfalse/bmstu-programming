.PHONY: all build fuzz meta clean

all: test

build:
	erlc test.erl

test: build fuzz meta

fuzz: build
	erl -noshell -s test main -- fuzz

meta: build
	erl -noshell -s test main -- meta

clean:
	rm -f *.beam
