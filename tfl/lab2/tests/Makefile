.PHONY: all build fuzz meta clean

all: test

build:
	erlc test.erl

test: build fuzz

fuzz: build
	erl -noshell -s test main -- fuzz

clean:
	rm -f *.beam
