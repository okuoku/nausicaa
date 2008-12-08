# prepare.sh --

set -x

#M4_FLAGS='--prefix-builtins'

# AUTOMAKE_LIBRARY=../infrastructure/amlocal.m4
# NAUSICAA_LIBRARY=../infrastructure/amnausicaa.m4
# AM_LIBRARIES="${AUTOMAKE_LIBRARY} ${NAUSICAA_LIBRARY}"
# MAKEFILE_INPUT=Makefile.am
# MAKEFILE_OUTPUT=Makefile.in

(cd ..
#    m4 ${M4_FLAGS} ${AM_LIBRARIES} ${MAKEFILE_INPUT} >${MAKEFILE_OUTPUT}

    if test \
        configure.ac -nt configure -o \
        ../infrastructure/aclocal.m4 -nt configure
        then
        autoconf
    fi)

../configure \
    --enable-fasl \
    --enable-ypsilon \
    "$@"

##    --enable-larceny

### end of file
