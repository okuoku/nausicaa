# Makefile --
#
# Makefile for foof-loop.


#page
## ------------------------------------------------------------
## Global, package specific, variables.
## ------------------------------------------------------------

PACKAGE			= nausicaa-foof-loop
VERSION			= 0.1d0
PKG_DIR			= $(PACKAGE)/$(VERSION)
PKG_ID			= $(PACKAGE)-$(VERSION)

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Global stuff.
## ------------------------------------------------------------

# Customise this file for the installation directories.
#-include Makefile.custom

include ../infrastructure/Makefile.begin

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Rules.
## ------------------------------------------------------------

.PHONY: all clean test install uninstall

all:

clean:

test:
	IKARUS_LIBRARY_PATH=$(PWD) $(SCHEME_SCRIPT) test.sls

install:

uninstall:

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Done.
## ------------------------------------------------------------

include ../infrastructure/Makefile.begin


### end of file
# Local Variables:
# fill-column: 60
# End:
