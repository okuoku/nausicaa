# @configure_input@
#

nausicaa_ENABLE_POSIX	= @nausicaa_ENABLE_POSIX@
nausicaa_ENABLE_RANDOM	= @nausicaa_ENABLE_RANDOM@

## --------------------------------------------------------------------

ifeq (yes,$(strip $(nausicaa_ENABLE_POSIX)))

posix_SRCDIR	= $(srcdir)/src/posix
posix_BUILDDIR	= $(builddir)/posix-objects.d

posix_PREREQUISITES = $(wildcard $(posix_SRCDIR)/*.h)

$(eval $(call ds-c-library,posix))

endif

## --------------------------------------------------------------------

ifeq (yes,$(strip $(nausicaa_ENABLE_RANDOM)))

random_SRCDIR	= $(srcdir)/src/random
random_BUILDDIR	= $(builddir)/random-objects.d

random_PREREQUISITES = $(wildcard $(random_SRCDIR)/*.h)

$(eval $(call ds-c-library,random))

endif

## --------------------------------------------------------------------

ifneq ($(strip $(file)),)
random_tests_PATTERNS			= test-*$(file)*.c
else
random_tests_PATTERNS			= test-*.c
endif
random_tests_PREREQUISITES		= $(wildcard $(random_SRCDIR)/*.h)
random_tests_CC_COMPILE_INCLUDES	= -I$(random_SRCDIR)
random_tests_programs_CC_PROGRAM_LDFLAGS= -L$(random_shlib_BUILDDIR)
random_tests_programs_CC_PROGRAM_LIBS	= -l$(random_LIBRARY_ID)

$(eval $(call ds-c-test-programs,random))

tests_ENV	= LD_LIBRARY_PATH=$(random_shlib_BUILDDIR)

tests:
	$(foreach f,$(random_tests_programs_PATHNAMES),\
	$(tests_ENV) $(f);)


### end of file
# Local Variables:
# mode: makefile-gmake
# End:
