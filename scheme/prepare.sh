# prepare.sh --

set -x

(cd ..
    if test \
        configure -ot prepare.sh   -o \
        configure -ot configure.ac -o \
        configure -ot aclocal.m4   -o \
        configure -ot infrastructure/develstuff.autoconf
        then
        autoconf
    fi)

../configure \
    --config-cache                              \
    --with-abi=local-slackware                  \
    --enable-use-sudo                           \
    --enable-fasl                               \
    --enable-ikarus                             \
    --enable-larceny                            \
    --enable-mosh                               \
    --enable-ypsilon                            \
    --enable-binfmt                             \
    "$@"



### end of file
