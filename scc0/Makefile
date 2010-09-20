# Makefile for scc0, the bootstrap compiler.  This assumes a
# working non-bootstrap version

.PHONY: all clean install revert

SCHEME_PATH=../scheme-core

SCC=${SCHEME_PATH}/vcsh --compile --initial-package:scheme

all: update

update:
	cp ${SCHEME_PATH}/scheme.scf .
	cp ${SCHEME_PATH}/compiler-run.scf .

%.scf: %.scm Makefile
	$(SCC) --cross-compile --output:$@ $<

clean: 
	rm -f *.scf *~

revert:
	git checkout compiler-run.scf scheme.scf
