# Makefile --
#

GUILE		= guile
LARCENY		= larceny
MOSH		= mosh
PETITE		= petite
RACKET		= plt-r6rs
YPSILON		= ypsilon
VICARE		= vicare

GUILE_FLAGS	= -l guile-r6rs-setup.scm --auto-compile -s

.PHONY: all test gtest ltest mtest ptest vtest ytest rtest

all:

test: gtest ltest mtest ptest vtest ytest rtest

gtest:
	GUILE_LOAD_PATH=. $(GUILE) $(GUILE_FLAGS) the-program.sps

ltest:
	LARCENY_LIBPATH=. $(LARCENY) -r6rs -program the-program.sps

mtest:
	MOSH_LOADPATH=. $(MOSH) the-program.sps

ptest:
	CHEZSCHEMELIBDIRS=. CHEZSCHEMELIBEXTS=.sls $(PETITE) --program the-program.sps

vtest:
	VICARE_LIBRARY_PATH=. $(VICARE) --r6rs-script the-program.sps

ytest:
	YPSILON_SITELIB=. $(YPSILON) the-program.sps

rtest:
	$(RACKET) ++path $(srcdir) the-program.sps

### end of file
