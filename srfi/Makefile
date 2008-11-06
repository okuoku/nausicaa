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
FIND_DIRS	= cd libraries ; $(FIND) . -type d
FIND_FILES	= cd libraries ; $(FIND) . -type f	\
			-and -name '*.sls'		\
			-and -print
FIND_FASL	= cd libraries ; $(FIND) . -type f	\
			-and -name '*.ikarus-fasl'	\
			-and -print

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Installation.
## ------------------------------------------------------------

DIRECTORIES		:= $(sort $(shell $(FIND_DIRS)))
REVERSED_DIRECTORIES	:= $(shell cd libraries; ls -1d $(DIRECTORIES) | $(SORT) --reverse)
FILES			:= $(shell $(FIND_FILES))
FASL_FILES		:= $(shell $(FIND_FASL))

.PHONY: all compile compile-clean clean install uninstall

all:

clean: compile-clean

install:
	$(INSTALL_DIR) $(foreach d,$(DIRECTORIES),$(DESTDIR)$(pkglibdir)/$(d))
	cd libraries; $(foreach f,$(FILES),\
		$(INSTALL_DATA) $(f) $(DESTDIR)$(pkglibdir)/$(dir $(f));)
	cd libraries; $(foreach f,$(FASL_FILES),\
		$(INSTALL_DATA) $(f) $(DESTDIR)$(pkglibdir)/$(dir $(f));)

uninstall:
	-@$(RM) $(addprefix $(DESTDIR)$(pkglibdir)/,$(FILES))
	-@$(RM) $(addprefix $(DESTDIR)$(pkglibdir)/,$(FASL_FILES))
	-@$(RMDIR) $(addprefix $(DESTDIR)$(pkglibdir)/,$(REVERSED_DIRECTORIES))

## ------------------------------------------------------------

compile:
	cd libraries ; IKARUS_LIBRARY_PATH=$(PWD)/srfi \
	$(IKARUS) --compile-dependencies srfi/compile-all.sls

compile-clean:
	-cd libraries; $(RM) $(FASL_FILES)

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
