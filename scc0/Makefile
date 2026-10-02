# Makefile for scc0, the bootstrap compiler.  This assumes a
# working non-bootstrap version

.PHONY: all update clean revert

SCHEME_PATH=../scheme-core

all: update

# Copies the current images, after checking that they reproduce
# themselves (stage 2 = stage 3; see scheme-core/Makefile).
update:
	$(MAKE) -C .. bootstrap-check
	cp ${SCHEME_PATH}/scheme.scf .
	cp ${SCHEME_PATH}/compiler-run.scf .

clean: 
	rm -f *.scf *~

revert:
	git checkout compiler-run.scf scheme.scf
