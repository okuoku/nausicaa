# Makefile --
#
# Installation   rules   for   GNU   Readline  wrapper   around   Scheme
# implementations.

#page
## ------------------------------------------------------------
## Global variables.
## ------------------------------------------------------------

srcdir		= $(PWD)
builddir	= "$(PWD)/=build"

#page
## ------------------------------------------------------------
## Data.
## ------------------------------------------------------------

srfi_MODULES = \
			srfi_and_let_star		\
			srfi_args_fold			\
			srfi_case_lambda		\
			srfi_cond_expand		\
			srfi_eager_comp			\
			srfi_environment_variables	\
			srfi_error_reporting		\
			srfi_general_cond		\
			srfi_let_values 		\
			srfi_lightweight_testing	\
			srfi_list			\
			srfi_parameters			\
			srfi_random			\
			srfi_rec			\
			srfi_receive			\
			srfi_records			\
			srfi_specialize_procedures	\
			srfi_streams			\
			srfi_string_ports		\
			srfi_strings			\
			srfi_time			\
			srfi_vector

ikarus_MODULES	= ikarus ikarus_ffi	$(srfi_MODULES)
ypsilon_MODULES	= ypsilon		$(srfi_MODULES)
larceny_MODULES	= larceny		$(srfi_MODULES)
mosh_MODULES	=			$(srfi_MODULES)
vicare_MODULES	= vicare vicare_ffi	$(srfi_MODULES)
petite_MODULES	=			$(srfi_MODULES)

#page
## ------------------------------------------------------------
## Wrapper installation directories.
## ------------------------------------------------------------

prefix		= $(HOME)

ifeq ($(strip $(prefix)),)
prefix		= ~
endif

bindir		= $(prefix)/bin
pkgdatadir	= $(prefix)

#page
## ------------------------------------------------------------
## Programs.
## ------------------------------------------------------------

SED			= sed
IKARUS_SED_FLAGS	= -e "s|@pkgdatadir@|$(pkgdatadir)|g"
LARCENY_SED_FLAGS	= -e "s|@pkgdatadir@|$(pkgdatadir)|g"
MOSH_SED_FLAGS		= -e "s|@pkgdatadir@|$(pkgdatadir)|g"
PETITE_SED_FLAGS	= -e "s|@pkgdatadir@|$(pkgdatadir)|g"
VICARE_SED_FLAGS	= -e "s|@pkgdatadir@|$(pkgdatadir)|g"
YPSILON_SED_FLAGS	= -e "s|@pkgdatadir@|$(pkgdatadir)|g"

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

SORT		= sort --unique

#page
## ------------------------------------------------------------
## Rules.
## ------------------------------------------------------------

ikarus_COMPLETIONS	= completions/r6rs_identifiers \
  $(foreach m,$(ikarus_MODULES),completions/$(m)_completions)
ypsilon_COMPLETIONS	= completions/r6rs_identifiers \
  $(foreach m,$(ypsilon_MODULES),completions/$(m)_completions)
larceny_COMPLETIONS	= completions/r6rs_identifiers \
  $(foreach m,$(larceny_MODULES),completions/$(m)_completions)
mosh_COMPLETIONS	= completions/r6rs_identifiers \
  $(foreach m,$(mosh_MODULES),completions/$(m)_completions)
petite_COMPLETIONS	= completions/r6rs_identifiers \
  $(foreach m,$(petite_MODULES),completions/$(m)_completions)
vicare_COMPLETIONS	= completions/r6rs_identifiers \
  $(foreach m,$(vicare_MODULES),completions/$(m)_completions)

SCRIPTS		= $(builddir)/ike $(builddir)/ype \
		  $(builddir)/lae $(builddir)/moe \
		  $(builddir)/pee $(builddir)/vie
ikarus_COMPFILE	= $(builddir)/.ikarus_completions
ypsilon_COMPFILE= $(builddir)/.ypsilon_completions
larceny_COMPFILE= $(builddir)/.larceny_completions
mosh_COMPFILE	= $(builddir)/.mosh_completions
petite_COMPFILE	= $(builddir)/.petite_completions
vicare_COMPFILE	= $(builddir)/.vicare_completions
COMPFILES	= $(ikarus_COMPFILE)  $(ypsilon_COMPFILE) \
		  $(larceny_COMPFILE) $(mosh_COMPFILE)	 \
		  $(petite_COMPFILE)  $(vicare_COMPFILE)

.PHONY: all clean install install-bin install-dat

all: builddir
	$(SED) $(IKARUS_SED_FLAGS)  < scripts/ike.in > $(builddir)/ike
	$(SED) $(LARCENY_SED_FLAGS) < scripts/lae.in > $(builddir)/lae
	$(SED) $(MOSH_SED_FLAGS)    < scripts/moe.in > $(builddir)/moe
	$(SED) $(PETITE_SED_FLAGS)  < scripts/pee.in > $(builddir)/pee
	$(SED) $(VICARE_SED_FLAGS)  < scripts/vie.in > $(builddir)/vie
	$(SED) $(YPSILON_SED_FLAGS) < scripts/ype.in > $(builddir)/ype
	cat $(ikarus_COMPLETIONS)  | $(SORT) >$(ikarus_COMPFILE)
	cat $(larceny_COMPLETIONS) | $(SORT) >$(larceny_COMPFILE)
	cat $(mosh_COMPLETIONS)    | $(SORT) >$(mosh_COMPFILE)
	cat $(petite_COMPLETIONS)  | $(SORT) >$(petite_COMPFILE)
	cat $(vicare_COMPLETIONS)  | $(SORT) >$(vicare_COMPFILE)
	cat $(ypsilon_COMPLETIONS) | $(SORT) >$(ypsilon_COMPFILE)

clean:
	-$(RM) $(builddir)

builddir:
	-@test -d $(builddir) || $(MKDIR) $(builddir)

## ------------------------------------------------------------

install: install-bin install-data

install-bin:
	test -d $(DESTDIR)$(bindir) || $(INSTALL_DIR) $(DESTDIR)$(bindir)
	$(INSTALL_BIN) $(SCRIPTS) $(DESTDIR)$(bindir)

install-data:
	test -d $(DESTDIR)$(pkgdatadir) || $(INSTALL_DIR) $(DESTDIR)$(pkgdatadir)
	$(INSTALL_DATA) $(COMPFILES) $(DESTDIR)$(pkgdatadir)

## ------------------------------------------------------------

uninstall: uninstall-bin uninstall-data

uninstall-bin:
	-$(RM) $(addprefix $(DESTDIR)$(bindir)/,$(notdir $(SCRIPTS)))
	-$(RMDIR) $(DESTDIR)$(bindir)

uninstall-data:
	-$(RM) $(addprefix $(DESTDIR)$(pkgdatadir)/,$(notdir $(COMPFILES)))
	-$(RMDIR) $(DESTDIR)$(pkgdatadir)

## ------------------------------------------------------------

.PHONY: abi abu

abi: all install

abu: all uninstall install

### end of file
