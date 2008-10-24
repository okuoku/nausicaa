# Makefile --
#
# Makefile for SRFI under Ikarus.

#page
## ------------------------------------------------------------
## Global, package specific, variables.
## ------------------------------------------------------------

PACKAGE			= mykarus-srfi
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
## Programs.
## ------------------------------------------------------------

SED		= sed
SUDO		= sudo

FIND		= find
FIND_DIRS	= $(FIND) srfi -type d
FIND_FILES	= $(FIND) srfi -type f \
			-and -not -name '*.sps' \
			-and -not -name '*~'	\
			-and -print

## ------------------------------------------------------------

INSTALL			= install

INSTALL_DIR_MODE	= 0755
INSTALL_BIN_MODE	= 0555
INSTALL_DATA_MODE	= 0444

INSTALL_DIR		= $(INSTALL) -m $(INSTALL_DIR_MODE) -d
INSTALL_BIN		= $(INSTALL) -m $(INSTALL_BIN_MODE)
INSTALL_DATA		= $(INSTALL) -m $(INSTALL_DATA_MODE)

## ------------------------------------------------------------

RM		= rm --force --recursive --verbose --
RMDIR		= rmdir --parents --ignore-fail-on-non-empty --
MKDIR		= mkdir --parents --verbose

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
