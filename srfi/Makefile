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
FIND_FILES	= $(FIND) srfi -type f			\
			-and -not -name '*.sps'		\
			-and -not -name '*~'		\
			-and -print
FIND_FASL	= $(FIND) srfi -type f			\
			-and -name '*.ikarus-fasl'	\
			-and -print

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Installation.
## ------------------------------------------------------------

DIRECTORIES		:= $(sort $(shell $(FIND_DIRS)))
REVERSED_DIRECTORIES	:= $(shell ls -1d $(DIRECTORIES) | $(SORT) --reverse)
FILES			:= $(shell $(FIND_FILES))
FASL_FILES		:= $(shell $(FIND_FASL))

.PHONY: all clean install uninstall

all:
	IKARUS_LIBRARY_PATH=$(srcdir)/srfi \
	$(IKARUS) --compile-dependencies compile-all.ikarus.sps

clean:
	-$(RM) $(FASL_FILES)

install:
	$(INSTALL_DIR) $(foreach d,$(DIRECTORIES),$(DESTDIR)$(pkglibdir)/$(d))
	$(foreach f,$(FILES),\
		$(INSTALL_DATA) $(f) $(DESTDIR)$(pkglibdir)/$(dir $(f));)
	$(foreach f,$(FASL_FILES),\
		$(INSTALL_DATA) $(f) $(DESTDIR)$(pkglibdir)/$(dir $(f));)

uninstall:
	-@$(RM) $(addprefix $(DESTDIR)$(pkglibdir)/,$(FILES))
	-@$(RM) $(addprefix $(DESTDIR)$(pkglibdir)/,$(FASL_FILES))
	-@$(RMDIR) $(addprefix $(DESTDIR)$(pkglibdir)/,$(REVERSED_DIRECTORIES))

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
