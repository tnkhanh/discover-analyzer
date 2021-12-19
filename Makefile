.DEFAULT_GOAL := all

all:
	dune build src/Discover.exe
	@echo ""
	cp -f _build/default/src/Discover.exe discover
	dune build src/Benchmark.exe
	cp -f _build/default/src/Benchmark.exe benchmark

clean:
	dune clean

# Auto-format code
format:
	-dune build @fmt
	dune promote
