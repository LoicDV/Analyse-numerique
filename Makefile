# Ce makefile délègue le travail de compilation à Dune.  Il faut
# donc avoir installé ce dernier avec « opam install dune ».

CURRENT_DIR = $(notdir $(shell pwd))
TARBALL	    = TP.tgz

all build:
	dune build @exe

run:
	dune exec src/main.exe

test:
	dune runtest

# Make a tarball.  "dune-project" is excluded because it does not
# contain any special instructions and will output an error with older
# versions of Dune.
tar dist: clean
	touch "$(TARBALL)"
	cd .. && tar --dereference --exclude="*~" --exclude="*.tgz" \
	  --exclude="._*" --exclude=".DS_Store" --exclude "_build" \
	  --exclude="doc/*.html" --exclude="doc/*.css" \
	  -zcvf "$(CURRENT_DIR)/$(TARBALL)" "$(CURRENT_DIR)"

clean::
	dune clean
	-$(RM) -r $(wildcard *~ *.tar.gz)

.PHONY: all build run test tar dist clean