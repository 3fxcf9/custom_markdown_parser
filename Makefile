default:
	opam update
	opam install . --deps-only
	dune build

build:
	dune build

install:
	opam update
	opam install --yes . --deps-only

# lint:
# 	dune build @lint
# 	dune build @fmt

# test:
# 	dune runtest

clean:
	dune clean
	git clean -dfX
	rm -rf _docs/

doc:
	make clean
	dune build @doc
	mkdir _docs/
	cp	-r ./_build/default/_doc/_html/* _docs/

format:
	dune build @fmt --auto-promote

hook:
	cp ./_hooks/* .git/hooks

# coverage:
# 	make clean
# 	BISECT_ENABLE=yes dune build
# 	dune runtest
# 	bisect-ppx-report html
# 	bisect-ppx-report summary
