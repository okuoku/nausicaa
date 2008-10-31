# Makefile --
#
# Makefile for SRFI under Ikarus.

#page
## ------------------------------------------------------------
## Global, package specific, variables.
## ------------------------------------------------------------

PACKAGE			= nausicaa-srfi
VERSION			= 0.1d0
PKG_DIR			= $(PACKAGE)/$(VERSION)
PKG_ID			= $(PACKAGE)-$(VERSION)

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Custom variables.
## ------------------------------------------------------------

# Customise this file for the installation directories.
include Makefile.custom

include ../infrastructure/Makefile.begin

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Programs for this package.
## ------------------------------------------------------------

FIND		= find
FIND_DIRS	= $(FIND) srfi -type d
FIND_FILES	= $(FIND) srfi -type f \
			-and -not -name '*.sps' \
			-and -not -name '*~'	\
			-and -print

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Installation.
## ------------------------------------------------------------

DIRECTORIES		:= $(shell $(FIND_DIRS))
FILES			:= $(shell $(FIND_FILES))

.PHONY: all install

all:

install:
	$(INSTALL_DIR) $(foreach d,$(DIRECTORIES),$(DESTDIR)$(pkglibdir)/$(d))
	$(foreach f,$(FILES),\
		$(INSTALL_DATA) $(f) $(DESTDIR)$(pkglibdir)/$(dir $(f));)

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Done.
## ------------------------------------------------------------

include ../infrastructure/Makefile.end

### end of file
# Local Variables:
# fill-column: 60
# End:
