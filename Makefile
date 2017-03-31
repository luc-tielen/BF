
EXECUTABLE=$(shell find -type f | grep BF-exe | tail -1)

build:
	stack build

clean:
	stack clean

test: build
	${EXECUTABLE} ./test/fixtures/test.bf

.PHONY: build clean test run

