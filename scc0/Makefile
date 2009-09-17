# Makefile for scc0, the bootstrap compiler.  This assumes a
# working non-bootstrap version

.PHONY: all clean install revert

SCHEME_PATH=../scheme-core

SCC=${SCHEME_PATH}/vcsh --compile --initial-package:scheme

all: update

update: fasl-compiler0.scf invoke-repl.scf
	cp ${SCHEME_PATH}/scheme.scf .
	cp ${SCHEME_PATH}/fasl-compiler.scf .
	cp ${SCHEME_PATH}/fasl-compiler-run.scf .

%.scf: %.scm Makefile
	$(SCC) --cross-compile --output:$@ $<

invoke-repl.scf: invoke-repl.scm

fasl-compiler0.scf: fasl-compiler0.scm

clean: 
	rm -f *.scf *~

revert:
	git revert fasl-compiler-run.scf \
               fasl-compiler.scf \
               fasl-compiler0.scf \
               invoke-repl.scf \
               scheme.scf
