.PHONY: all discover test clean format
.DEFAULT_GOAL := all

all: discover test

discover:
	@echo ""
	dune build src/Discover.exe
	cp -f _build/default/src/Discover.exe discover

test:
	@echo ""
	dune build src/Test_benchmark.exe
	cp -f _build/default/src/Test_benchmark.exe test_benchmark

clean:
	dune clean

# Auto-format code
format:
	-dune build @fmt
	dune promote
