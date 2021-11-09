# mli:
# 	 dune exec -- ocaml-print-intf src/frontend/discover.ml

all:
	dune build src/Discover.exe
	@echo ""
	cp -f _build/default/src/Discover.exe discover

clean:
	dune clean
